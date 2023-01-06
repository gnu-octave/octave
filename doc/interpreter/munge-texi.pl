#!/usr/bin/perl -w

########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

# Validate program call
die "usage: munge-texi TOP-SRCDIR DOCSTRING-FILE < file" if (@ARGV < 2);

$top_srcdir = shift (@ARGV);

# Constant patterns
$doc_delim = qr/^\x{1d}/;
$tex_delim = qr/\Q-*- texinfo -*-\E/;
$comment_line = qr/^\s*(?:$|#)/;
# Pre-declare hash size for efficiency
keys(%help_text) = 1800;

################################################################################
# Load DOCSTRINGS into memory while expanding @seealso references
foreach $DOCSTRING_file (@ARGV)
{
  ## DOCSTRINGS files may exist in the current (build) directory OR
  ## in the source directory when building from a release.
  $DOCSTRING_file_srcdir = "$top_srcdir/$DOCSTRING_file";

  open (DOCFH, $DOCSTRING_file) or open (DOCFH, $DOCSTRING_file_srcdir)
    or die "Unable to open $DOCSTRING_file\n";

  # Skip comments
  while (defined ($_ = <DOCFH>) and /$comment_line/o) {;}

  # Validate DOCSTRING file format
  die "invalid doc file format\n" if (! /$doc_delim/o);

  do
  {
    s/\s*$//;   # strip EOL character(s)
    $symbol = substr ($_, 1);
    $docstring = extract_docstring ();
    if ($help_text{$symbol})
    {
      warn "$DOCSTRING_file:$.:warning: ignoring duplicate entry for $symbol\n";
    }
    else
    {
      $help_text{$symbol} = $docstring;
    }

  } while (! eof);

}

################################################################################
# Process .txi to .texi by expanding @DOCSTRING, @EXAMPLEFILE macros

# Add warning header
print '@c DO NOT EDIT!  Generated automatically by munge-texi.pl.',"\n\n";

TXI_LINE: while (<STDIN>)
{
  s'@seealso'@xseealso'g;

  if (/^\s*\@DOCSTRING\((\S+)\)/)
  {
    $func = $1;
    $docstring = $help_text{$func};
    if (! $docstring)
    {
      warn "warning: no DOCSTRING entry for $func\n";
      next TXI_LINE;
    }

    $func =~ s/^@/@@/;   # Texinfo uses @@ to produce '@'
    $func =~ s/\./_/g;   # Texinfo doesn't like '.' in node names
    $docstring =~ s/^$tex_delim$/\@anchor{XREF$func}/m;
    print $docstring,"\n";

    next TXI_LINE;
  }
  if (/^\s*\@EXAMPLEFILE\((\S+)\)/)
  {
    $fname = "$top_srcdir/examples/code/$1";
    print '@verbatim',"\n";
    open (EXAMPFH, $fname) or die "unable to open example file $fname\n";
    while (<EXAMPFH>)
    {
      print $_;
      print "\n" if (eof and substr ($_, -1) ne "\n");
    }
    close (EXAMPFH);
    print '@end verbatim',"\n";

    next TXI_LINE;
  }

  # pass ordinary lines straight through to output
  print $_;
}


################################################################################
# Subroutines
################################################################################
sub extract_docstring
{
  my ($docstring, $arg_list, $func_list, $repl, $rest_of_line);

  while (defined ($_ = <DOCFH>) and ! /$doc_delim/o)
  {
    # expand any @seealso references
    if (m'^@seealso\{')
    {
      # join multiple lines until full macro body found
      while (! /}/m) { $_ .= <DOCFH>; }

      ($arg_list, $rest_of_line) = m'^@seealso\{(.*)\}(.*)?'s;

      $func_list = $arg_list;
      $func_list =~ s/\s+//gs;
      $repl = "";
      foreach $func (split (/,/, $func_list))
      {
        $func =~ s/^@/@@/;     # Texinfo uses @@ to produce '@'
        $node = $func;
        $node =~ s/\./_/g;     # Texinfo doesn't like '.' in node names
        $repl .= "\@ref{XREF$node,,$func}, ";
      }
      substr($repl,-2) = "";   # Remove last ', '
      # write out @xseealso because we have our own macro that conflicts
      # with the one introduced in Texinfo 6.
      $_ = "\@xseealso{$repl}$rest_of_line";
    }

    $docstring .= $_;
  }

  return $docstring;
}
