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
## @deftypefn  {} {@var{y} =} meansq (@var{x})
## @deftypefnx {} {@var{y} =} meansq (@var{x}, @var{dim})
## @deftypefnx {} {@var{y} =} meansq (@var{x}, @var{vecdim})
## @deftypefnx {} {@var{y} =} meansq (@var{x}, "all")
## @deftypefnx {} {@var{y} =} meansq (@dots{}, @var{nanflag})
## Compute the mean square of the elements of the vector @var{x}.
##
## The mean square is defined as
## @tex
## $$
## {\rm meansq} (x) = {\sum_{i=1}^N {x_i}^2 \over N}
## $$
## where $N$ is the number of elements of @var{x}.
##
## @end tex
## @ifnottex
##
## @example
## @group
## meansq (@var{x}) = 1/N SUM_i @var{x}(i)^2
## @end group
## @end example
##
## @noindent
## where @math{N} is the length of the @var{x} vector.
##
## @end ifnottex
## If @var{x} is a vector, then @code{meansq (@var{x})} returns the mean square
## of the elements in @var{x}.
##
## If @var{x} is a matrix, then @code{meansq (@var{x})} returns a row vector
## with each element containing the mean square of the corresponding column in
## @var{x}.
##
## If @var{x} is an array, then @code{meansq (@var{x})} computes the mean
## square along the first non-singleton dimension of @var{x}.
##
## The optional input @var{dim} specifies the dimension to operate on and must
## be a positive integer.  Specifying any singleton dimension of @var{x},
## including any dimension exceeding @code{ndims (@var{x})}, will return
## @code{@var{x}.^2}.
##
## Specifying the dimensions as @var{vecdim}, a vector of non-repeating
## dimensions, will return the mean square over the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim}
## greater than @code{ndims (@var{x})} is ignored.
##
## Specifying the dimension as @qcode{"all"} will cause @code{mean} to operate
## on all elements of @var{x}, and is equivalent to @code{meansq (@var{x}(:))}.
##
## The optional variable @var{nanflag} specifies whether to include or exclude
## NaN values from the calculation using any of the previously specified input
## argument combinations.  The default value for @var{nanflag} is
## @qcode{"includenan"} which keeps NaN values in the calculation.  To exclude
## NaN values set the value of @var{nanflag} to @qcode{"omitnan"}.  The output
## will still contain NaN values if @var{x} consists of all NaN values in the
## operating dimension.
##
## @seealso{var, std, moment}
## @end deftypefn

function y = meansq (x, varargin)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  ## Calculate sum of squares and run input validation on varargin.
  y = sumsq (x, varargin{:});

  ## Normalize
  omitnan = strcmpi (varargin, 'omitnan');
  if (any (omitnan))
    ## Only call isnan() when necessary because it is slow.
    varargin(omitnan) = [];
    y ./= sum (! isnan (x), varargin{:});

  else
    if (isempty (varargin))
      ## Find the first non-singleton dimension.
      (dim = find (size (x) != 1, 1)) || (dim = 1);
      norm = size (x, dim);
    elseif (isnumeric (varargin{1}))
      dim = varargin{1};
      norm = prod (size (x, dim));
    elseif (any (strcmpi (varargin, 'all')))
      norm = numel (x);
    else
      ## String option such as 'includenan' given, but no dimension.
      ## Find the first non-singleton dimension.
      (dim = find (size (x) != 1, 1)) || (dim = 1);
      norm = size (x, dim);
    endif

    y ./= norm;

  endif

endfunction


%!assert (meansq (1:5), 11)
%!assert (meansq (single (1:5)), single (11))
%!assert (meansq (magic (4)), [94.5, 92.5, 92.5, 94.5])
%!assert (meansq (magic (4), 2), [109.5; 77.5; 77.5; 109.5])
%!assert (meansq ([1 2], 3), [1 4])

## Test optional arguments DIM, 'all', 'omitnan'.
%!test
%! x = [-9:9];
%! y = [x;x+6;x-6];
%! assert (meansq (x), 30);
%! assert (meansq (y, 2), [30; 66; 66]);
%! assert (meansq (y, 'all'), 54);
%! y = y';
%! y(4,2) = NaN;
%! assert (meansq (y, 'omitnan'), [30, 1254/18, 66]);
%! assert (meansq (y, 'all'), NaN);
%! assert (meansq (y, 'all', 'includenan'), NaN);
%! assert (meansq (y, 'all', 'omitnan'), 3078/56);
%! exp = [30, NaN, 66];
%! assert (meansq (y), exp);
%! assert (meansq (y, 'includenan'), exp);
%! exp = [30, 1254/18, 66];
%! assert (meansq (y, 'omitnan'), exp);
%! assert (meansq (y', 2, 'omitnan'), exp');

## Test dimension indexing with vecdim in N-dimensional arrays
%!test
%! x = repmat ([1:20;6:25], [5 2 6 3]);
%! assert (size (meansq (x, [3 2])), [10 1 1 3]);
%! assert (size (meansq (x, [1 2])), [1 1 6 3]);
%! assert (size (meansq (x, [1 2 4])), [1 1 6]);
%! assert (size (meansq (x, [1 4 3])), [1 40]);
%! assert (size (meansq (x, [1 2 3 4])), [1 1]);

## Test exceeding dimensions
%!assert (meansq (2*ones (2,2), 3), 4*ones (2,2))
%!assert (meansq (2*ones (2,2,2), 99), 4*ones (2,2,2))
%!assert (meansq (magic (4), 3), (magic (4)).^2)
%!assert (meansq (magic (4), [1 3]), [94.5, 92.5, 92.5, 94.5])
%!assert (meansq (magic (4), [1 99]), [94.5, 92.5, 92.5, 94.5])

## Test results with vecdim in N-dimensional arrays and "omitnan"
%!test
%! x = repmat ([1:5;11:15], [5 2 6 3]);
%! m = repmat ([11; 171], [5 1 1 3]);
%! assert (meansq (x, [3 2]), m);
%! x(2,6,6,3) = NaN;
%! m(2,1,1,3) = 10139/59;
%! assert (meansq (x, [3 2], 'omitnan'), m);

## Test input validation
%!error <Invalid call> meansq ()
%!error <Invalid call> meansq (1,2,3,4)
## FIXME: Revisit these tests once sumsq input validation is updated.
%!#error <X must be a numeric> meansq (['A'; 'B'])
%!#error <DIM must be an integer> meansq (1, ones (2,2))
%!#error <DIM must be an integer> meansq (1, 1.5)
%!#error <DIM must be .* a valid dimension> meansq (1, 0)
