## Copyright (C) 1996, 1997 John W. Eaton
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

## usage: rot90 (x, k)
##
## Rotate the matrix x counterclockwise k*90 degrees.
##
## If the second argument is omitted, k is taken to be 1.
##
## See also: flipud, fliplr

## Author: jwe

function y = rot90 (x, k)

  if (nargin < 2)
    k = 1;
  endif

  if (imag (k) != 0 || fix (k) != k)
    error ("rot90: k must be an integer");
  endif

  if (nargin == 1 || nargin == 2)
    k = rem (k, 4);
    if (k < 0)
      k = k + 4;
    endif
    if (k == 0)
      y = x;
    elseif (k == 1)
      y = flipud (x.');
    elseif (k == 2)
      y = flipud (fliplr (x));
    elseif (k == 3)
      y = (flipud (x)).';
    else
      error ("rot90: internal error!");
    endif
  else
    usage ("rot90 (x [, k])");
  endif

endfunction
