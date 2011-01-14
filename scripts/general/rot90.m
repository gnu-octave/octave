## Copyright (C) 1993-2011 John W. Eaton
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
## @deftypefn {Function File} {} rot90 (@var{A}, @var{k})
## Return a copy of @var{A} with the elements rotated counterclockwise in
## 90-degree increments.  The second argument is optional, and specifies
## how many 90-degree rotations are to be applied (the default value is 1).
## Negative values of @var{k} rotate the matrix in a clockwise direction.
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

function B = rot90 (A, k)

  if (nargin == 1 || nargin == 2)
    if (nargin < 2)
      k = 1;
    endif

    if (ndims (A) > 2)
      error ("rot90: Only works with 2-D arrays");
    endif

    if (imag (k) != 0 || fix (k) != k)
      error ("rot90: K must be an integer");
    endif

    k = rem (k, 4);

    if (k < 0)
      k = k + 4;
    endif

    if (k == 0)
      B = A;
    elseif (k == 1)
      B = flipud (A.');
    elseif (k == 2)
      B = flipud (fliplr (A));
    elseif (k == 3)
      B = (flipud (A)).';
    else
      error ("rot90: internal error!");
    endif
  else
    print_usage ();
  endif

endfunction

%!test
%! x1 = [1, 2; 3, 4];
%! x2 = [2, 4; 1, 3];
%! x3 = [4, 3; 2, 1];
%! x4 = [3, 1; 4, 2];
%! 
%! assert(rot90 (x1) == x2);
%! assert(rot90 (x1, 2) == x3);
%! assert(rot90 (x1, 3) == x4);
%! assert(rot90 (x1, 4) == x1);
%! assert(rot90 (x1, 5) == x2);
%! assert(rot90 (x1, -1) == x4);

%% Test input validation
%!error rot90 ();
%!error rot90 (1, 2, 3);

