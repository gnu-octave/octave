## Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009 David Bateman
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
## @deftypefn {Function File} {} rotdim (@var{x}, @var{n}, @var{plane})
## Return a copy of @var{x} with the elements rotated counterclockwise in
## 90-degree increments.  The second argument is optional, and specifies
## how many 90-degree rotations are to be applied (the default value is 1).
## The third argument is also optional and defines the plane of the
## rotation.  As such @var{plane} is a two element vector containing two
## different valid dimensions of the matrix.  If @var{plane} is not given
## Then the first two non-singleton dimensions are used.
##
## Negative values of @var{n} rotate the matrix in a clockwise direction.
## For example,
##
## @example
## @group
## rotdim ([1, 2; 3, 4], -1, [1, 2])
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
## rotdim ([1, 2; 3, 4], -1, [1, 2])
## rotdim ([1, 2; 3, 4], 3, [1, 2])
## rotdim ([1, 2; 3, 4], 7, [1, 2])
## @end group
## @end example
## @seealso{rot90, flipud, fliplr, flipdim}
## @end deftypefn

function y = rotdim (x, k, plane)
  
  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (nargin > 1 && ! isempty(k))
    if (!isscalar (k) || imag (k) != 0 || fix (k) != k)
      error ("rotdim: k must be an scalar integer");
    endif
  else
    k = 1;
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin < 3)
    ## Find the first two non-singleton dimension.
    plane = [];
    dim = 0;
    while (dim < nd)
      dim = dim + 1;
      if (sz (dim) != 1)
        plane = [plane, dim];
        if (length (plane) == 2)
          break;
        endif
      endif
    endwhile
    if (length (plane) < 1)
      plane = [1, 2];
    elseif (length (plane) < 2)
      plane = [1, plane];
    endif
  else
    if (! (isvector (plane) && length (plane) == 2
           && all (plane == round (plane)) && all (plane > 0)
           && all (plane < (nd + 1)) && plane(1) != plane(2)))
      error ("rotdim: plane must be a 2 element integer vector defining a valid plane");
    endif
  endif

  k = rem (k, 4);
  if (k < 0)
    k = k + 4;
  endif
  if (k == 0)
    y = x;
  elseif (k == 2)
    y = flipdim (flipdim (x, plane(1)), plane(2));
  elseif (k == 1 || k == 3)
    perm = 1:nd;
    perm(plane(1)) = plane(2);
    perm(plane(2)) = plane(1);
    y = permute (x, perm);
    if (k == 1)
      y = flipdim (y, min (plane));
    else
      y = flipdim (y, max (plane));
    endif
  else
    error ("rotdim: internal error!");
  endif

endfunction
