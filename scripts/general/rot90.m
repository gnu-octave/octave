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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} rot90 (@var{x}, @var{n})
## Return a copy of @var{x} with the elements rotated counterclockwise in
## 90-degree increments.  The second argument is optional, and specifies
## how many 90-degree rotations are to be applied (the default value is 1).
## Negative values of @var{n} rotate the matrix in a clockwise direction.
## For example,
##
## @example
## @group
## rot90 ([1, 2; 3, 4], -1)
##      @result{}  3  1
##          4  2
## @end group
## @end example
##
## @noindent
## rotates the given matrix clockwise by 90 degrees.  The following are all
## equivalent statements:
##
## @example
## @group
## rot90 ([1, 2; 3, 4], -1)
## rot90 ([1, 2; 3, 4], 3)
## rot90 ([1, 2; 3, 4], 7)
## @end group
## @end example
##
## Due to the difficulty of defining an axis about which to rotate the 
## matrix @code{rot90} only work with 2-D arrays.  To rotate N-d arrays
## use @code{rotdim} instead.
## @seealso{rotdim, flipud, fliplr, flipdim}
## @end deftypefn

## Author: jwe

function y = rot90 (x, k)

  if (nargin == 1 || nargin == 2)
    if (nargin < 2)
      k = 1;
    endif

    if (ndims (x) > 2)
      error ("rot90: Only works with 2-D arrays")
    endif

    if (imag (k) != 0 || fix (k) != k)
      error ("rot90: k must be an integer");
    endif

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
    usage ("rot90 (x, k)");
  endif

endfunction
