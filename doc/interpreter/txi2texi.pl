#!/usr/bin/perl -w

########################################################################
##
## Copyright (C) 2012-2026 The Octave Project Developers
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

################################################################################
# Purpose: This file reads raw documentation source from Octave files and
#          makes necessary changes to export conforming Texinfo .texi
#          files.
################################################################################

# Validate program call
die "usage: txi2texi TOP-SRCDIR DOCSTRING-FILE1 ... < file" if (@ARGV < 2);

$top_srcdir = shift (@ARGV);

# Constant regexp patterns
# Records in DOCSTRING file are separated by $doc_delim
$doc_delim = qr/^\x{1d}/;
# Texinfo source within record begins with $tex_delim
$tex_delim = qr/\Q-*- texinfo -*-\E/;
$comment_line = qr/^\s*(?:$|#)/;
# Pre-declare hash size for efficiency
keys(%help_text) = 1800;

################################################################################
# Load DOCSTRINGS into memory while expanding @seealso references
foreach $DOCSTRING_file (@ARGV)
{
  # DOCSTRINGS files may exist in the current (build) directory
  # OR in the source directory when building from a release tarball.
  open ($DOCFH, "<", $DOCSTRING_file)
    or open ($DOCFH, "<", "$top_srcdir/$DOCSTRING_file")
      or die "Unable to open $DOCSTRING_file\n";

  # Skip comments
  while (defined ($_ = <$DOCFH>) and /$comment_line/o) {;}

  # Validate DOCSTRING file format
  die "File $DOCSTRING_file: invalid file format\n" if (! /$doc_delim/o);

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
print '@c DO NOT EDIT!  Generated automatically by txi2texi.pl.',"\n\n";

TXI_LINE: while (<STDIN>)
{
  # Texinfo introduced incompatible @seealso macro.  Replace all occurrences
  # with Octave-defined @xseealso macro that has desired behavior.
  s'@seealso'@xseealso'g;

  if (m'^\s*@DOCSTRING\((\S+)\)')
  {
    $fcn = $1;
    $docstring = $help_text{$fcn};
    if (! $docstring)
    {
      warn "warning: no DOCSTRING entry for $fcn\n";
      next TXI_LINE;
    }

    $fcn =~ s/^@/@@/;   # Texinfo uses @@ to produce '@'
    $fcn =~ s/\./_/g;   # Texinfo doesn't like '.' in node names
    # Replace texinfo start tag by an anchor.  QtHelp requires a string
    # directly following the anchor.  Adding "&nbsp;" in html mode adds an
    # additional vertical space which is compensated by span-tag with
    # negative top margin.
    $docstring =~ s/^$tex_delim$/\@anchor{XREF$fcn}\n\@html\n<span style=\"display:block; margin-top:-4.5ex;\">&nbsp;<\/span>\n\@end html\n\n/m;
    print $docstring,"\n";

    next TXI_LINE;
  }
  if (m'^\s*@EXAMPLEFILE\((\S+)\)')
  {
    $fname = "$top_srcdir/examples/code/$1";
    print '@verbatim',"\n";
    open ($EXAMPFH, "<", $fname) or die "unable to open example file $fname\n";
    while (<$EXAMPFH>)
    {
      print $_;
      print "\n" if (eof and substr ($_, -1) ne "\n");
    }
    close ($EXAMPFH);
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
  my ($docstring, $arg_list, $fcn, $fcn_list, $node, $repl, $rest_of_line);

  while (defined ($_ = <$DOCFH>) and ! /$doc_delim/o)
  {
    # expand any @seealso references
    if (m'^@seealso\{')
    {
      # join multiple lines until full macro body found
      while (! /}/m) { $_ .= <$DOCFH>; }

      ($arg_list, $rest_of_line) = m'^@seealso\{(.*)\}(.*)?'s;

      $fcn_list = $arg_list;
      $fcn_list =~ s/\s+//gs;
      $repl = "";
      foreach $fcn (split (/,/, $fcn_list))
      {
        $fcn =~ s/^@/@@/;     # Texinfo uses @@ to produce '@'
        $node = $fcn;
        $node =~ s/\./_/g;     # Texinfo doesn't like '.' in node names
        $repl .= "\@ref{XREF$node,,$fcn}, ";
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
