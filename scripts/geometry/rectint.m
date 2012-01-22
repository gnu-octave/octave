## Copyright (C) 2008-2012 Bill Denney
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
## matrix where the i-th row corresponds to the i-th row of a and the j-th
## column corresponds to the j-th row of b.
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
    error ("rectint: A must have 4 columns");
  elseif (columns (b) != 4)
    error ("rectint: B must have 4 columns");
  elseif any ([a(:,3:4);b(:,3:4)](:) < 0)
    error ("rectint: all widths and heights must be > 0");
  endif

  ## This runs faster if the number of rows of a is greater than the
  ## number of rows of b.  Swap them and transpose to make it run
  ## faster.
  swapinputs = false ();
  if (rows (a) > rows (b))
    tmp = a;
    a = b;
    b = tmp;
    swapinputs = true ();
  endif

  area = zeros (rows (a), rows (b));
  r1 = [a(:,1:2) a(:,1:2)+a(:,3:4)];
  r2 = [b(:,1:2) b(:,1:2)+b(:,3:4)];
  for i = 1:columns (area)
    ## Find the location of each point relative to the other points.
    r1x1small = r1(:,1) < r2(i,1);
    r1x1large = r1(:,1) > r2(i,3);
    r1x1mid = (r1(:,1) >= r2(i,1)) & (r1(:,1) <= r2(i,3));
    r1x2small = r1(:,3) < r2(i,1);
    r1x2large = r1(:,3) > r2(i,3);
    r1x2mid = (r1(:,3) >= r2(i,1)) & (r1(:,3) <= r2(i,3));

    r1y1small = r1(:,2) < r2(i,2);
    r1y1large = r1(:,2) > r2(i,4);
    r1y1mid = (r1(:,2) >= r2(i,2)) & (r1(:,2) <= r2(i,4));
    r1y2small = r1(:,4) < r2(i,2);
    r1y2large = r1(:,4) > r2(i,4);
    r1y2mid = (r1(:,4) >= r2(i,2)) & (r1(:,4) <= r2(i,4));

    ## determine the width of the rectangle
    ## r1 completely encloses r2
    area(r1x1small & r1x2large,i) = r2(i,3) - r2(i,1);
    ## the range goes from r2x min to r1x max
    mask = r1x1small & r1x2mid;
    area(mask,i) = r1(mask,3) - r2(i,1);
    ## the range goes from r1x min to r2x max
    mask = r1x1mid & r1x2large;
    area(mask,i) = r2(i,3) - r1(mask,1);
    ## the range goes from r1x min to r1x max
    mask = r1x1mid & r1x2mid;
    area(mask,i) = r1(mask,3) - r1(mask,1);

    ## determine the height of the rectangle
    ## r1 completely encloses r2
    area(r1y1small & r1y2large,i) .*= r2(i,4) - r2(i,2);
    ## the range goes from r2y min to r1y max
    mask = r1y1small & r1y2mid;
    area(mask,i) .*= r1(mask,4) - r2(i,2);
    ## the range goes from r1y min to r2y max
    mask = r1y1mid & r1y2large;
    area(mask,i) .*= r2(i,4) - r1(mask,2);
    ## the range goes from r1x min to r1x max
    mask = r1y1mid & r1y2mid;
    area(mask,i) .*= r1(mask,4) - r1(mask,2);

  endfor

  if swapinputs
    area = area';
  endif

endfunction

## Tests
## Exactly overlapping
%!assert(rectint([0 0 1 1], [0 0 1 1]), 1)
## rect2 completely enclosed by rect1
%!assert(rectint([-1 -1 3 3], [0 0 1 1]), 1)
## rect1 completely enclosed by rect2
%!assert(rectint([0 0 1 1], [-1 -1 3 3]), 1)
## rect1 right and top in rect2
%!assert(rectint([-1 -1 1.5 1.5], [0 0 1 1]), 0.25)
## rect2 right and top in rect1
%!assert(rectint([0 0 1 1], [-1 -1 1.5 1.5]), 0.25)
## no overlap - shared corner
%!assert(rectint([0 0 1 1], [1 1 2 2]), 0)
## no overlap - shared edge
%!assert(rectint([0 0 1 1], [0 1 2 2]), 0)
## Correct orientation of output
%!assert(rectint([0 0 1 1;0.5 0.5 1 1;-1 -1 2 2], [1 1 2 2]), [0;0.25;0])
%!assert(rectint([1 1 2 2], [0 0 1 1;0.5 0.5 1 1;-1 -1 2 2]), [0 0.25 0])
