#! /usr/bin/perl -w

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

unless (@ARGV > 1) { die "Usage: $0 SRCDIR src-file1 ..." }

$srcdir = shift (@ARGV);

print <<__END_OF_MSG__;
### DO NOT EDIT!
###
### This file is generated automatically from Octave source files.
### Edit source files directly and run make to update this file.

__END_OF_MSG__

FILE: foreach $fname (@ARGV)
{
  if (-f "$fname")
  {
    $src_fname = "$fname";
  }
  else
  {
    $src_fname = "$srcdir/$fname";
  }

  open (SRC_FH, $src_fname) or die "Unable to open $src_fname";

  @func_list = ();
  @docstr = ();

  LINE: while (my $line = <SRC_FH>)
  {
    if ($line =~ /^\s*DEF(?:METHOD|METHOD_(|STATIC_)DLD|METHODX|METHODX_(|STATIC_)DLD|UN|UN_(|STATIC_)DLD|UNX|UNX_(|STATIC_)DLD)\s*\(/)
    {
      ($func) = $line =~ /\("?(\w+)"?,/;
      unless ($func) { die "Unable to parse $src_fname at line $.\n" }
      push (@func_list, $func);

      ## Skip optional line that declares list of classes that this
      ## function accepts.
      $line = <SRC_FH>;
      if ($line =~ m#\s*classes:#)
      {
        $line = <SRC_FH>;
      }

      if ($line =~ m#\s*doc:\s+\Q/*\E\s+\Q-*- texinfo -*-\E\s*$#)
      {
        $str = "-*- texinfo -*-\n";
        $reading_docstring = 1;
      }
      else
      {
        print STDERR "mk-doc.pl: undocumented function $func from $fname\n";
        push (@docstr, "Undocumented.");
      }
    }
    elsif ($reading_docstring)
    {
      if ($line =~ /^.*\s+\*\/\s*\)\s*$/)
      {
        $line =~ s#\s+\*/\s*\)\s*$##;
        push (@docstr, $str . $line);
        $reading_docstring = 0;
      }
      else
      {
        $str .= $line;
      }
    }
  }
  close (SRC_FH);

  ## Print results in DOCSTRING format
  foreach $i (0 .. $#func_list)
  {
    $func = $func_list[$i];
    print "\x{1d}$func\n";
    print "\@c $func $fname\n";
    print $docstr[$i],"\n";
  }

}
