########################################################################
##
## Copyright (C) 2018-2025 The Octave Project Developers
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
## @deftypefn  {} {[@var{s}, @var{l}] =} bounds (@var{x})
## @deftypefnx {} {[@var{s}, @var{l}] =} bounds (@var{x}, @var{dim})
## @deftypefnx {} {[@var{s}, @var{l}] =} bounds (@var{x}, @var{vecdim})
## @deftypefnx {} {[@var{s}, @var{l}] =} bounds (@var{x}, "all")
## @deftypefnx {} {[@var{s}, @var{l}] =} bounds (@dots{}, @var{nanflag})
## Return the smallest and largest values of the input data @var{x}.
##
## If @var{x} is a vector, then @code{bounds (@var{x})} returns the smallest
## and largest values of the elements in @var{x} in @var{s} and @var{l},
## respectively.
##
## If @var{x} is a matrix, then @code{bounds (@var{x})} returns the smallest
## and largest values for each column of @var{x} as row vectors @var{s} and
## @var{l}, respectively.
##
## If @var{x} is an array, then @code{bounds (@var{x})} computes the smallest
## and largest values along the first non-singleton dimension of @var{x}.
##
## The data in @var{x} must be numeric.  By default, any NaN values are ignored.
## The size of @var{s} and @var{l} is equal to the size of @var{x} except for
## the operating dimension, which becomes 1.
##
## The optional input @var{dim} specifies the dimension to operate on and must
## be a positive integer.  Specifying any singleton dimension of @var{x},
## including any dimension exceeding @code{ndims (@var{x})}, will return
## @code{@var{x}}.
##
## Specifying multiple dimensions with input @var{vecdim}, a vector of
## non-repeating dimensions, will operate along the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim}
## greater than @code{ndims (@var{x})} is ignored.
##
## Specifying the dimension as @qcode{"all"} will cause @code{bounds} to
## operate on all elements of @var{x}, and is equivalent to
## @code{bounds (@var{x}(:))}.
##
## The optional variable @var{nanflag} specifies whether to include or exclude
## NaN values from the calculation using any of the previously specified input
## argument combinations.  The default value for @var{nanflag} is
## @qcode{"omitnan"} which does not include NaN values in the result.  If the
## argument @qcode{"includenan"} is given, and there is a NaN present, then the
## result for both smallest (@var{s}) and largest (@var{l}) elements will be
## NaN.
##
## Usage Note: The bounds are a quickly computed measure of the dispersion of a
## data set, but are less accurate than @code{iqr} if there are outlying data
## points.
## @seealso{range, iqr, mad, std}
## @end deftypefn

function [s, l] = bounds (x, varargin)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! (isnumeric (x)))
    error ("bounds: X must be a numeric array");
  endif

  if (isempty (varargin))
    s = min (x);
    l = max (x);
  else
    s = min (x, [], varargin{:});
    l = max (x, [], varargin{:});
  endif

endfunction


%!test
%! [s,l] = bounds (1:10);
%! assert ([s,l], [1, 10]);
%!test
%! [s,l] = bounds ([10:-1:1]');
%! assert ([s,l], [1, 10]);
%!test
%! [s,l] = bounds (single (1:10));
%! assert ([s,l], single ([1, 10]));
%!assert (bounds (magic (3)), [3, 1, 2])
%!assert (bounds (magic (3), 2), [1; 3; 2])
%!test
%! x = magic (3);
%! x(2,3) = NaN;
%! assert (bounds (x), [3, 1, 2]);
%! assert (bounds (x, "omitnan"), [3, 1, 2]);
%! assert (bounds (x, "includenan"), [3, 1, NaN]);
%! assert (bounds (x, 2), [1; 3; 2]);
%! assert (bounds (x, 2, "omitnan"), [1; 3; 2]);
%! assert (bounds (x, 2, "includenan"), [1; NaN; 2]);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! [s,l] = bounds (x, 3);
%! assert (s, x(:,:,1));
%! assert (l, x(:,:,3));
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! [s,l] = bounds (x, [2, 3]);
%! assert (s, x(:,1,1));
%! assert (l, x(:,3,3));
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! [s,l] = bounds (x, [1, 3]);
%! assert (s, x(1,:,1));
%! assert (l, x(3,:,3));
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! [s,l] = bounds (x, [1, 2, 3]);
%! assert (s, 1);
%! assert (l, 27);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! [s,l] = bounds (x, "all");
%! assert (s, 1);
%! assert (l, 27);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! x(3) = NaN;
%! [s,l] = bounds (x, "all");
%! assert (s, 1);
%! assert (l, 27);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! x(3) = NaN;
%! [s,l] = bounds (x, "all", "omitnan");
%! assert (s, 1);
%! assert (l, 27);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! x(3) = NaN;
%! [s,l] = bounds (x, "all", "includenan");
%! assert (s, NaN);
%! assert (l, NaN);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! x(3) = NaN;
%! [s,l] = bounds (x, "includenan");
%! assert (s, min (x, [], "includenan"));
%! assert (l, max (x, [], "includenan"));
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! x(3) = NaN;
%! [s,l] = bounds (x, 3, "includenan");
%! assert (s, x(:,:,1));
%! assert (l(3), NaN);

## Test input validation
%!error <Invalid call> bounds ()
%!error <Invalid call> bounds (1, 2, 3, 4)
%!error <bounds: X must be a numeric array> bounds (['A'; 'B'])
%!error <bounds: X must be a numeric array> bounds ([true; false])
%!error <bounds: X must be a numeric array> bounds ({1, 2})
