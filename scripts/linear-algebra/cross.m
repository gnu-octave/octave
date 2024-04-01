########################################################################
##
## Copyright (C) 1995-2024 The Octave Project Developers
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

## -*- texinfo -*-
## @deftypefn  {} {@var{z} =} cross (@var{x}, @var{y})
## @deftypefnx {} {@var{z} =} cross (@var{x}, @var{y}, @var{dim})
## Compute the vector cross product of two 3-dimensional vectors @var{x} and
## @var{y}.
##
## If @var{x} and @var{y} are arrays, the cross product is applied along the
## first dimension with three elements.
##
## The optional argument  @var{dim} forces the cross product to be calculated
## along the specified dimension.  An error will be produced if the specified
## dimension is not three elements in size.
##
## Example Code:
##
## @example
## @group
## cross ([1, 1, 0], [0, 1, 1])
##   @result{}
##        1  -1   1
## @end group
## @end example
##
## @example
## @group
## cross (magic (3), eye (3), 2)
##   @result{}
##        0   6  -1
##       -7   0   3
##        9  -4   0
## @end group
## @end example
##
## @seealso{dot, curl, divergence}
## @end deftypefn

function z = cross (x, y, dim)

  if (nargin < 2)
    print_usage ();
  endif

  nd = ndims (x);

  if (nd < 3 && ndims (y) < 3 && nargin < 3)
    ## COMPATIBILITY -- opposite behavior for cross(row,col)
    ## Swap x and y in the assignments below to get the matlab behavior.
    ## Better yet, fix the calling code so that it uses conformant vectors.
    if (iscolumn (x) && isrow (y))
      warning ("cross: taking cross product of column by row");
      y = y.';
    elseif (isrow (x) && iscolumn (y))
      warning ("cross: taking cross product of row by column");
      x = x.';
    endif
  endif

  sz = size (x);

  if (nargin == 2)
     dim = find (sz == 3, 1);
     if (isempty (dim))
       error ("cross: must have at least one dimension with 3 elements");
     endif
  else
    if (! (isnumeric (dim) && dim > 0 && isreal (dim) && ...
          isscalar (dim) && dim == fix (dim)))
      error ("cross: DIM must be a positive scalar whole number");
    endif

    if (dim > nd || sz(dim) != 3)
      error ("cross: must have three elements in dimension DIM");
    endif
  endif

  idx2 = idx3 = idx1 = {':'}(ones (1, nd));
  idx1(dim) = 1;
  idx2(dim) = 2;
  idx3(dim) = 3;

  if (size_equal (x, y))
    x1 = x(idx1{:});
    x2 = x(idx2{:});
    x3 = x(idx3{:});
    y1 = y(idx1{:});
    y2 = y(idx2{:});
    y3 = y(idx3{:});
    z = cat (dim, (x2.*y3 - x3.*y2), (x3.*y1 - x1.*y3), (x1.*y2 - x2.*y1));
  else
    error ("cross: X and Y must have the same dimensions");
  endif

endfunction


%!test
%! x = [1, 0, 0];
%! y = [0, 1, 0];
%! r = [0, 0, 1];
%! assert (cross (x, y), r, eps);

%!test
%! x = [1, 2, 3];
%! y = [4, 5, 6];
%! r = [(2*6-3*5), (3*4-1*6), (1*5-2*4)];
%! assert (cross (x, y), r, eps);

%!test
%! x = [1, 0, 0; 0, 1, 0; 0, 0, 1];
%! y = [0, 1, 0; 0, 0, 1; 1, 0, 0];
%! r = [0, 0, 1; 1, 0, 0; 0, 1, 0];
%! assert (cross (x, y, 2), r, eps);
%! assert (cross (x, y, 1), -r, eps);

%!test <*65527>
%! x = cat (3, [1, 1, 1]', [1, 1, 1]');
%! y = cat (3, [1, 0, 0], [1, 0, 0]);
%! fail ("cross (x, y)", "X and Y must have the same dimensions");
%! fail ("cross (y, x)", "X and Y must have the same dimensions");

## Test input validation
%!error <Invalid call> cross ()
%!error <Invalid call> cross (1)
%!error <must have at least one dimension with 3 elements> cross (0, 0)
%!error <must have at least one dimension with 3 elements> cross ([1, 2], [3, 4])
%!error <must have at least one dimension with 3 elements> cross ([1, 2], [3, 4, 5])
%!error <must have three elements in dimension DIM> cross (0, 0, 1)
%!error <must have three elements in dimension DIM> cross ([1, 2, 3], [1, 2, 3], 1)
%!error <must have three elements in dimension DIM> cross ([1, 2, 3], [1, 2, 3], 9)
%!error <must have three elements in dimension DIM> cross (magic (3), magic (3), 4)
%!error <DIM must be a positive scalar whole number> cross ([1, 2, 3], [4, 5, 6], {1})
%!error <DIM must be a positive scalar whole number> cross ([1, 2, 3], [4, 5, 6], "a")
%!error <DIM must be a positive scalar whole number> cross ([1, 2, 3], [4, 5, 6], true)
%!error <DIM must be a positive scalar whole number> cross ([1, 2, 3], [4, 5, 6], [1, 2])
%!error <DIM must be a positive scalar whole number> cross ([1, 2, 3], [4, 5, 6], 0)
%!error <DIM must be a positive scalar whole number> cross ([1, 2, 3], [4, 5, 6], -1)
%!error <DIM must be a positive scalar whole number> cross ([1, 2, 3], [4, 5, 6], 1.5)
%!error <DIM must be a positive scalar whole number> cross ([1, 2, 3], [4, 5, 6], 2i)
%!error <X and Y must have the same dimensions> cross ([1, 2, 3], [3, 4])
%!warning <taking cross product of column by row> cross ([1, 2, 3]', [4, 5, 6]);
%!warning <taking cross product of row by column> cross ([1, 2, 3], [4, 5, 6]');
