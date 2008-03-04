## Copyright (C) 2008 Bill Denney
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
## @deftypefn {Function File} {@var{area} =} rectint (@var{a}, @var{b})
##
## Compute the area of intersection of rectangles in @var{a} and
## rectangles in @var{b}.  Rectangles are defined as [x y width height]
## where x and y are the minimum values of the two orthogonal
## dimensions.
##
## If @var{a} or @var{b} are matrices, then the output, @var{area}, is a
## matrix where the ith row corresponds to the ith row of a and the jth
## column corresponds to the jth row of b.
##
## @seealso{polyarea}
## @end deftypefn

## Author: Bill Denney <bill@denney.ws>

function area = rectint (a, b)
	
  if (nargin != 2)
    print_usage ();
  elseif (ndims (a) != 2 || ndims (b) != 2)
    error ("rectint: expecting arguments to be 2-d arrays");
  elseif (columns (a) != 4)
    error ("rectint: a must have 4 columns");
  elseif (columns (b) != 4)
    error ("rectint: b must have 4 columns");
  endif

  area = zeros (rows (a), rows (b));
  for i = 1:rows (area)
    r1 = a(i,:);
    for j = 1:columns (area)
      r2 = b(j,:);
      area(i,j) = (overlap (r1([1, 3]), r2([1, 3]))
		   * overlap (r1([2, 4]), r2([2, 4])));
    endfor
  endfor

endfunction

function amt = overlap (r1, r2)

  ## Determine whether two ranges overlap.  Ranges are given as [value
  ## offset]
  amt = 0;

  ## Make sure that the values are in order.
  r1 = sort ([r1(1), r1(1)+r1(2)]);
  r2 = sort ([r2(1), r2(1)+r2(2)]);

  ## Is the first point in range 1 in the middle of range 2?
  p1 = sum (r1(1) <= r2) == 1;
  ## is the second?
  p2 = sum (r1(2) <= r2) == 1;
  if (p1)
    if (p2)
      amt = r1(2) - r1(1);
    else
      amt = r2(2) - r1(1);
    endif
  elseif (sum (r1(2) < r2) == 1)
    amt = r1(2) - r2(1);
  endif

endfunction

## Tests
%!assert(rectint([0 0 1 1], [1 1 2 2]), 0)
%!assert(rectint([0 0 1 1], [0.5 0.5 2 2]), 0.25)
%!assert(rectint([0 0 1 1;0.5 0.5 1 1], [1 1 2 2]), [0;0.25])
%!assert(rectint([1 1 2 2], [0 0 1 1;0.5 0.5 1 1]), [0 0.25])

