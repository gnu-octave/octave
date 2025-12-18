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
## @deftypefn  {} {@var{y} =} rms (@var{x})
## @deftypefnx {} {@var{y} =} rms (@var{x}, @var{dim})
## @deftypefnx {} {@var{y} =} rms (@var{x}, @var{vecdim})
## @deftypefnx {} {@var{y} =} rms (@var{x}, "all")
## @deftypefnx {} {@var{y} =} rms (@dots{}, @var{nanflag})
## Compute the root mean square of the input data @var{x}.
##
## The root mean square is defined as
## @tex
## $$
## {\rm rms} (x) = {\sqrt{{1 \over N} \sum_{i=1}^N {|x_i|}^2}}
## $$
## where $N$ is the number of elements of @var{x}.
##
## @end tex
## @ifnottex
##
## @example
## @group
## rms (@var{x}) = sqrt (1/N SUM_i @var{x}(i)^2)
## @end group
## @end example
##
## @noindent
## where @math{N} is the length of the @var{x} vector.
##
## @end ifnottex
## If @var{x} is a vector, then @code{rms (@var{x})} returns the root mean
## square of the elements in @var{x}.
##
## If @var{x} is a matrix, then @code{rms (@var{x})} returns a row vector
## with each element containing the root mean square of the corresponding
## column in @var{x}.
##
## If @var{x} is an array, then @code{rms (@var{x})} computes the root mean
## square along the first non-singleton dimension of @var{x}.
##
## The data in @var{x} must be numeric.  The size of @var{y} is equal to the
## size of @var{x} except for the operating dimension, which becomes 1.
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
## Specifying the dimension as @qcode{"all"} will cause @code{rms} to operate
## on all elements of @var{x}, and is equivalent to @code{rms (@var{x}(:))}.
##
## The optional variable @var{nanflag} specifies whether to include or exclude
## NaN values from the calculation using any of the previously specified input
## argument combinations.  The default value for @var{nanflag} is
## @qcode{"includenan"} which keeps NaN values in the calculation.  To exclude
## NaN values set the value of @var{nanflag} to @qcode{"omitnan"}.  The output
## will still contain NaN values if @var{x} consists of all NaN values in the
## operating dimension.
##
## @seealso{meansq, var, std, moment}
## @end deftypefn

function y = rms (x, varargin)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  ## Piggyback on existing code 
  y = sqrt (meansq (x, varargin{:}));

endfunction


%!assert (rms (1:5), sqrt (11))
%!assert (rms (single (1:5)), single (sqrt (11)))
%!assert (rms (magic (4)), sqrt ([94.5, 92.5, 92.5, 94.5]))
%!assert (rms (magic (4), 2), sqrt ([109.5; 77.5; 77.5; 109.5]))
%!assert (rms ([1 2], 3), sqrt ([1 4]))

## Test optional arguments DIM, 'all', 'omitnan'.
%!test
%! x = [-9:9];
%! y = [x;x+6;x-6];
%! assert (rms (x), sqrt (30));
%! assert (rms (y, 2), sqrt ([30; 66; 66]));
%! assert (rms (y, 'all'), sqrt (54));
%! y = y';
%! y(4,2) = NaN;
%! assert (rms (y, 'omitnan'), sqrt ([30, 1254/18, 66]));
%! assert (rms (y, 'all'), NaN);
%! assert (rms (y, 'all', 'includenan'), NaN);
%! assert (rms (y, 'all', 'omitnan'), sqrt (3078/56));
%! exp = sqrt ([30, NaN, 66]);
%! assert (rms (y), exp);
%! assert (rms (y, 'includenan'), exp);
%! exp = sqrt ([30, 1254/18, 66]);
%! assert (rms (y, 'omitnan'), exp);
%! assert (rms (y', 2, 'omitnan'), exp');

## Test dimension indexing with vecdim in N-dimensional arrays
%!test
%! x = repmat ([1:20;6:25], [5 2 6 3]);
%! assert (size (rms (x, [3 2])), [10 1 1 3]);
%! assert (size (rms (x, [1 2])), [1 1 6 3]);
%! assert (size (rms (x, [1 2 4])), [1 1 6]);
%! assert (size (rms (x, [1 4 3])), [1 40]);
%! assert (size (rms (x, [1 2 3 4])), [1 1]);

## Test exceeding dimensions
%!assert (rms (2*ones (2,2), 3), 2*ones (2,2))
%!assert (rms (2*ones (2,2,2), 99), 2*ones (2,2,2))
%!assert (rms (magic (4), 3), (magic (4)))
%!assert (rms (magic (4), [1 3]), sqrt ([94.5, 92.5, 92.5, 94.5]))
%!assert (rms (magic (4), [1 99]), sqrt ([94.5, 92.5, 92.5, 94.5]))

## Test results with vecdim in N-dimensional arrays and "omitnan"
%!test
%! x = repmat ([1:5;11:15], [5 2 6 3]);
%! m = repmat ([11; 171], [5 1 1 3]);
%! assert (rms (x, [3 2]), sqrt (m));
%! x(2,6,6,3) = NaN;
%! m(2,1,1,3) = 10139/59;
%! assert (rms (x, [3 2], 'omitnan'), sqrt (m));

## Test input validation
%!error <Invalid call> rms ()
%!error <Invalid call> rms (1,2,3,4)
%!error <X must be a numeric array> rms (['A'; 'B'])
%!error <X must be a numeric array> rms ([true, false])
%!error <X must be a numeric array> rms ({1, 2})
