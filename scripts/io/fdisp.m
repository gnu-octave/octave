## Copyright (C) 2000 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} fdisp (@var{fid}, @var{x})
## Display the value of @var{x} on file id @var{fid}.  For example,
##
## @example
## fdisp (stdout, "The value of pi is:"), fdisp (stdout, pi)
##
##      @print{} the value of pi is:
##      @print{} 3.1416
## @end example
##
## @noindent
## Note that the output from @code{fdisp} always ends with a newline.
## @end deftypefn
## @seealso{disp}

## Author: jwe

function fdisp (fid, x)

  if (nargin == 2)
    fid << x << "\n";
  else
    usage ("fdisp (fid, x)");
  endif

endfunction
