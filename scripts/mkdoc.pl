#! /usr/bin/perl -w
#
# Copyright (C) 2012 Rik Wehbring
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

## Expecting arguments in this order:
##
##  SRCDIR SRCDIR-FILES ... -- LOCAL-FILES ...

unless (@ARGV >= 2) { die "Usage: $0 srcdir m_filename1 ..." ; }

$srcdir = shift (@ARGV) . '/';

print <<__END_OF_MSG__;
### DO NOT EDIT!
###
### This file is generated automatically from Octave source files.
### Edit source files directly and run make to update this file.

__END_OF_MSG__

MFILE: foreach $m_fname (@ARGV)
{
  if ($m_fname eq "--")
  {
    $srcdir = "./";
    next MFILE;
  }

  $full_fname = $srcdir . $m_fname;
  next MFILE unless ( $full_fname =~ m{(.*)/(@|)([^/]*)/(.*)\.m} );
  if ($2)
    { $fcn = "$2$3/$4"; }
  else
    { $fcn = $4; }

  @help_txt = gethelp ($fcn, $full_fname);
  next MFILE if ($help_txt[0] eq "");

  print "$fcn\n";
  print "\@c $fcn scripts/$m_fname\n";

  foreach $_ (@help_txt)
  {
    s/^\s+\@/\@/ unless $in_example;
    s/^\s+\@group/\@group/;
    s/^\s+\@end\s+group/\@end group/;
    $in_example = (/\s*\@example\b/ .. /\s*\@end\s+example\b/);
    print $_;
  }
}

################################################################################
# Subroutines
################################################################################
sub gethelp
{
  ($fcn, $fname) = @_[0..1]; 
  open (FH, $fname) or return "";

  do
  {
    @help_txt = ();

    ## Advance to non-blank line
    while (defined ($_ = <FH>) and /^\s*$/) {;}

    if (! /^\s*(?:#|%)/ or eof (FH))
    {
      ## No comment block found.  Return empty string
      close (FH);
      return "";
    }

    ## Extract help text stopping when comment block ends
    do
    {
      ## Remove comment characters at start of line
      s/^\s*(?:#|%){1,2} ?//;
      push (@help_txt, $_);
    } until (! defined ($_ = <FH>) or ! /^\s*(?:#|%)/);

  } until ($help_txt[0] !~ /^(?:Copyright|Author)/); 

  close (FH);

  return @help_txt;
}
