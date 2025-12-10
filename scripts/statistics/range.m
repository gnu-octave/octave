########################################################################
##
## Copyright (C) 1995-2025 The Octave Project Developers
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
## @deftypefn  {} {@var{y} =} range (@var{x})
## @deftypefnx {} {@var{y} =} range (@var{x}, @var{dim})
## @deftypefnx {} {@var{y} =} range (@var{x}, @var{vecdim})
## @deftypefnx {} {@var{y} =} range (@var{x}, "all")
## @deftypefnx {} {@var{y} =} range (@dots{}, @var{nanflag})
## Return the difference between the maximum and the minimum values
## of the input data @var{x}.
##
## If @var{x} is a vector, then @code{range (@var{x})} returns the difference
## between the smallest and largest values of the elements in @var{x}.
##
## If @var{x} is a matrix, then @code{range (@var{x})} returns a row vector
## @var{y} with the difference between the smallest and largest values for each
## column of @var{x}.
##
## If @var{x} is an array, then @code{range (@var{x})} computes the difference
## between the smallest and largest values along the first non-singleton
## dimension of @var{x}.
##
## The data in @var{x} must be numeric.  By default, any NaN values are ignored.
## The size of @var{r} is equal to the size of @var{x} except for the operating
## dimension, which becomes 1.
##
## The optional input @var{dim} specifies the dimension to operate on and must
## be a positive integer.  Specifying any singleton dimension of @var{x},
## including any dimension exceeding @code{ndims (@var{x})}, will return
## @code{@var{x}}.
##
## Specifying the dimensions as @var{vecdim}, a vector of non-repeating
## dimensions, will return the differences between smallest and largest values
## over the array slice defined by @var{vecdim}.  If @var{vecdim} indexes all
## dimensions of @var{x}, then it is equivalent to the option @qcode{"all"}.
## Any dimension in @var{vecdim} greater than @code{ndims (@var{x})} is ignored.
##
## Specifying the dimension as @qcode{"all"} will cause @code{range} to operate
## on all elements of @var{x}, and is equivalent to @code{range (@var{x}(:))}.
##
## The optional variable @var{nanflag} specifies whether to include or exclude
## NaN values from the calculation using any of the previously specified input
## argument combinations.  The default value for @var{nanflag} is
## @qcode{"omitnan"} which does not include NaN values in the result.  If the
## argument @qcode{"includenan"} is given, and there is a NaN present, then the
## corresponding result will be NaN.
##
## Usage Note: The range is a quickly computed measure of the dispersion of a
## data set, but is less accurate than @code{iqr} if there are outlying data
## points.
## @seealso{bounds, iqr, mad, std}
## @end deftypefn

function y = range (x, varargin)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("range: X must be a numeric array");
  endif

  if (isempty (varargin))
    y = max (x) - min (x);
  else
    y = max (x, [], varargin{:}) - min (x, [], varargin{:});
  endif

endfunction


%!assert (range (1:10), 9)
%!assert (range (single (1:10)), single (9))
%!assert (range (magic (3)), [5, 8, 5])
%!assert (range (magic (3), 2), [7; 4; 7])
%!assert (range (2), 0)
%!test
%! x = magic (3);
%! x(2,3) = NaN;
%! assert (range (x), [5, 8, 4]);
%! assert (range (x, "omitnan"), [5, 8, 4]);
%! assert (range (x, "includenan"), [5, 8, NaN]);
%! assert (range (x, 2), [7; 2; 7]);
%! assert (range (x, 2, "omitnan"), [7; 2; 7]);
%! assert (range (x, 2, "includenan"), [7; NaN; 7]);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! assert (range (x, 3), x(:,:,3) - x(:,:,1));
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! assert (range (x, [2, 3]), x(:,3,3) - x(:,1,1));
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! assert (range (x, [1, 3]), x(3,:,3) - x(1,:,1));
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! assert (range (x, [1, 2, 3]), 26);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! assert (range (x, "all"), 26);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! x(3) = NaN;
%! assert (range (x, "all"), 26);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! x(3) = NaN;
%! assert (range (x, "all", "omitnan"), 26);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! x(3) = NaN;
%! assert (range (x, "all", "includenan"), NaN);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! x(3) = NaN;
%! s = min (x, [], "includenan");
%! l = max (x, [], "includenan");
%! assert (range (x, "includenan"), l - s);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! x(3) = NaN;
%! y = range (x, 3, "includenan");
%! assert (y(3), NaN);

## Test input validation
%!error <Invalid call> range ()
%!error <Invalid call> range (1, 2, 3, 4)
%!error <range: X must be a numeric array> range (['A'; 'B'])
%!error <range: X must be a numeric array> range ({1, 2})
