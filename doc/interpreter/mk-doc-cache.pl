#!/usr/bin/perl

########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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

# This script is based on the old mk_doc_cache.m file.

use strict;
use warnings;

use File::Spec;
use File::Temp;

my $doc_delim = "\x{1d}";
my $tex_delim_ptn = qr/\Q-*- texinfo -*-\E/;

## Returns a File::Temp object with Texinfo code.
sub make_texinfo_file
{
  my $srcdir = shift;
  my $macro_fpath = shift;
  my @docstrings = @_;

  my $tmpfile = File::Temp->new (UNLINK => 1);

  ## Only the first file is the macro file.  Copy its contents verbatim.
  open (my $FH_macro, "<", $macro_fpath)
    or die "Unable to open $macro_fpath for reading: $!\n";
  while (<$FH_macro>) {  print {$tmpfile} $_;  }
  close ($FH_macro);

  foreach my $filepath (@docstrings)
    {
      ## DOCSTRINGS files may exist in the current (build) directory or in
      ## the source directory when building from a release.
      if (! -e $filepath)
        {
          ## Only triggered when re-building doc-cache outside source
          ## tree, from released sources.
          $filepath = File::Spec->catfile ($srcdir, $filepath);
        }
      open (my $FH, "<", $filepath)
        or die "Unable to open $filepath for reading: $!\n";

      my $in_header = 1;
      while (my $line = <$FH>)
        {
          ## DOCSTRINGS header ends once we find the first function.
          if ($in_header && $line =~ m/^$doc_delim/)
            {
              $in_header = 0;
            }
          next if ($in_header);
          next if ($line =~ /$tex_delim_ptn/);

          ## Change @seealso to private @xseealso macro
          if ($line =~ m'@seealso')
          {
            ## Combine @seealso macro spread over multiple lines
            while ($line !~ m/}$/) {  $line .= <$FH>;  }

            ## escape @ characters in old-style class names like @ftp
            $line =~ s/\@(\w)/\@\@$1/g;
            $line =~ s'@@seealso'@xseealso';
          }

          ## escape {}@ characters in Texinfo anchor name (e.g., @ftp/dir.m)
          $line =~ s/([{}@])/\@$1/g if ($line =~ m/^$doc_delim/);

          print {$tmpfile} $line;
        }
      close ($FH);
    }

  print {$tmpfile} $doc_delim;

  $tmpfile->flush ();

  return $tmpfile;
}

sub get_info_text
{
  my $texi_path = shift;

  my $makeinfo_command = "makeinfo --no-headers --no-warn --force --no-validate --fill-column=1024 $texi_path";
  my $info_text = `$makeinfo_command`;

  die "Unable to start makeinfo command '$makeinfo_command'"
    if (! defined $info_text);

  die "makeinfo produced no output!"
    if (! $info_text);

  return $info_text;
}

sub split_info
{
  my $info_text = shift;

  ## Constant patterns.  We only check for two underscores at the end,
  ## and not at the start, to also skip @class/__method__
  my $private_name_ptn = qr/__$/;

  my @formatted = ();

  my $beg_idx = index ($info_text, $doc_delim);
  while ($beg_idx >= 0)
    {
      my $end_idx = index ($info_text, $doc_delim, $beg_idx+1);
      if ($end_idx < 1)
        {
          $beg_idx = -1;
          next;
        }
      my $block = substr ($info_text, $beg_idx+1, $end_idx-$beg_idx-1);
      $beg_idx = $end_idx;

      my ($symbol, $doc) = split (/[\r\n]/, $block, 2);

      next if (length ($symbol) > 2 && $symbol =~ m/$private_name_ptn/);

      if (! defined ($doc))
      {
        warn "mk-doc-cache.pl: function '$symbol' may be undocumented";
        next;
      }

      $doc =~ s/^[\r\n]+//;
      next if (! $doc);

      (my $tmp = $doc) =~ s/^[\r\n]*  *-- .*[\r\n]//mg;
      next if (! $tmp);

      (my $first_sentence = $tmp) =~ s/(\.|[\r\n][\r\n]).*/$1/s;
      $first_sentence =~ s/([\r\n]| {2,})/ /g;
      $first_sentence =~ s/   *$/ /g;
      $first_sentence =~ s/^ +//;

      push (@formatted, [($symbol, $doc, $first_sentence)]);
    }

  return @formatted;
}

sub print_element
{
  my ($str) = @_;
  my $len = length ($str);

  print <<__END_OF_ELEMENT__;
# name: <cell-element>
# type: sq_string
# elements: 1
# length: $len
$str\n\n
__END_OF_ELEMENT__
}

sub print_cache
{
  my $num = @_;

  print <<__END_OF_CACHE_HDR__;
# created by mk-doc-cache.pl
# name: cache
# type: cell
# rows: 3
# columns: $num
__END_OF_CACHE_HDR__

  foreach my $elt (@_)
    {
      my $symbol = $elt->[0];
      my $doc = $elt->[1];
      my $first_sentence = $elt->[2];

      print_element ($symbol);
      print_element ($doc);
      print_element ($first_sentence);
      print "\n";
    }
}

## FIXME: This is a very C/C++ way of coding things.
## Perl convention would just be to have the executable code at end of file.
sub main
{
  my $srcdir = shift;
  my $macro_texi = shift;
  my @docstrings = @_;
  ## Everything else left in @_ are DOCSTRINGS files

  die "usage: mk_doc_cache SRCDIR MACRO-FILE DOCSTRINGS-FILE ..."
    if (@docstrings < 1);

  my $texi_file = make_texinfo_file ($srcdir, $macro_texi, @docstrings);

  my $info_text = get_info_text ($texi_file->filename);

  my @cache_blocks = split_info ($info_text);

  print_cache (@cache_blocks);
}

main (@ARGV);
