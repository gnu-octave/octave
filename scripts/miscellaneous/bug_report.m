## Copyright (C) 1994-2012 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} bug_report ()
## Display information about how to submit bug reports for Octave.
## @end deftypefn

## Author: jwe

function bug_report ()

  puts ("\n");
  puts ("  Bug reports play an essential role in making Octave\n");
  puts ("  reliable.  Please use the Octave bug tracker at\n");
  puts ("\n");
  puts ("    http://bugs.octave.org\n");
  puts ("\n");
  puts ("  to report problems.\n");
  puts ("\n");
  puts ("  Please also read the bug reporting guidelines at\n");
  puts ("\n");
  puts ("    http://www.octave.org/bugs.html\n");
  puts ("\n");
  puts ("  to learn how to submit useful bug reports that will\n");
  puts ("  help the Octave community diagnose and fix the problem\n");
  puts ("  quickly and efficiently.\n");
  puts ("\n");

endfunction

## Mark file as being tested.  No real test needed for this function.
%!assert (1)
