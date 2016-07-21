#!/usr/bin/perl -w
#
# Copyright (C) 2016 John W. Eaton
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

# This script is based on the old mk_doc_cache.m file.

use File::Temp qw(tempfile);

## Validate program call.

die "usage: mk_doc_cache OUTPUT-FILE SRCDIR MACRO-FILE ... -- DOCSTRINGS-FILE ..." if (@ARGV < 3);

$makeinfo_command = "makeinfo --no-headers --no-warn --force --no-validate --fill-column=1024";

$output_file = shift (@ARGV);
$top_srcdir = shift (@ARGV);

## Constant patterns.

$doc_delim = "\x{1d}";
$doc_delim_pat = qr/^\x{1d}/;
$tex_delim_pat = qr/\Q-*- texinfo -*-\E/;
$private_name_pat = qr/^__.+__/;

$text = "";

$macro_file = 1;

foreach $arg (@ARGV)
{
  if ($arg eq "--")
    {
      $macro_file = 0;
      next;
    }

  $file = $arg;

  ## DOCSTRINGS files may exist in the current (build) directory or in
  ## the source directory when building from a release.

  $file_srcdir = "$top_srcdir/$file";

  open (FH, $file) or open (FH, $file_srcdir)
    or die "Unable to open $file or $file_srcdir\n";

  $in_header = 1;

  while (<FH>)
    {
      if ($macro_file)
        {
          ## Copy contents verbatim.

          $text .= $_;
        }
      else
        {
          if ($in_header && /$doc_delim_pat/)
            {
              $in_header = 0;
            }

          next if ($in_header);

          next if (/$tex_delim_pat/);

          ## Escapes for symbol names.

          s/$doc_delim_pat(([#%]|)[{}]|@)/$doc_delim@$1/;
          $text .= $_;
        }
    }
}

$text .= $doc_delim;

($fh, $file) = tempfile (UNLINK => 1);
print $fh "$text";
close ($fh);

$cmd = "$makeinfo_command $file";
open (CMD, "-|", $cmd) or die "Unable to start makeinfo command $cmd\n";
$formatted_text = "";
while (<CMD>)
{
  $formatted_text .= $_;
}
close (CMD);

if (! $formatted_text)
{
  die "makeinfo produced no output!\n";
}

@formatted = ();

$beg_idx = index ($formatted_text, $doc_delim);
while ($beg_idx >= 0)
{
  $end_idx = index ($formatted_text, $doc_delim, $beg_idx+1);
  if ($end_idx < 1)
    {
      $beg_idx = -1;
      next;
    }
  $block = substr ($formatted_text, $beg_idx+1, $end_idx-$beg_idx-1);
  $beg_idx = $end_idx;

  ($symbol, $doc) = split (/[\r\n]/, $block, 2);

  next if (length ($symbol) > 2 && $symbol =~ m/$private_name_pat/);

  $doc =~ s/^[\r\n]+//;
  next if (! $doc);

  ($tmp = $doc) =~ s/^[\r\n]*  *-- .*[\r\n]//mg;
  next if (! $tmp);

  ($first_sentence = $tmp) =~ s/(\.|[\r\n][\r\n]).*/$1/s;
  $first_sentence =~ s/([\r\n]| {2,})/ /g;
  $first_sentence =~ s/   *$/ /g;
  $first_sentence =~ s/^ +//;

  push (@formatted, [($symbol, $doc, $first_sentence)]);
}

$num = @formatted;

print_preamble ($output_file, $num);

foreach $elt (@formatted)
{
  $symbol = $elt->[0];
  $doc = $elt->[1];
  $first_sentence = $elt->[2];

  print_element ($symbol);
  print_element ($doc);
  print_element ($first_sentence);
  print "\n";
}

sub print_preamble
{
  my ($output_file, $num) = @_;

  print "# $output_file created by mk-doc-cache.pl\n";
  print "# name: cache\n";
  print "# type: cell\n";
  print "# rows: 3\n";
  print "# columns: $num\n";
}

sub print_element
{
  my ($str) = @_;

  $len = length ($str);

  print "# name: <cell-element>\n";
  print "# type: sq_string\n";
  print "# elements: 1\n";
  print "# length: $len\n";
  print "$str\n\n\n";
}
