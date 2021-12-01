########################################################################
##
## Copyright (C) 1996-2021 The Octave Project Developers
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
## @deftypefn  {} {} std (@var{x})
## @deftypefnx {} {} std (@var{x}, @var{w})
## @deftypefnx {} {} std (@var{x}, @var{w}, @var{dim})
## @deftypefnx {} {} std (@var{x}, @var{w}, @qcode{"ALL"})
## Compute the standard deviation of the elements of the vector @var{x}.
##
## The standard deviation is defined as
## @tex
## $$
## {\rm std} (x) = \sigma = \sqrt{{\sum_{i=1}^N (x_i - \bar{x})^2 \over N - 1}}
## $$
## where $\bar{x}$ is the mean value of @var{x} and $N$ is the number of elements of @var{x}.
## @end tex
## @ifnottex
##
## @example
## @group
## std (@var{x}) = sqrt ( 1/(N-1) SUM_i (@var{x}(i) - mean(@var{x}))^2 )
## @end group
## @end example
##
## @noindent
## where @math{N} is the number of elements of the @var{x} vector.
## @end ifnottex
##
## If @var{x} is an array, compute the standard deviation for each column and
## return them in a row vector (or for an n-D array, the result is returned as
## an array of dimension 1 x n x m x @dots{}).
##
## The optional argument @var{w} determines the weighting scheme to use.  Valid
## values are:
##
## @table @asis
## @item 0 [default]:
## Normalize with @math{N-1}.  This provides the square root of the best
## unbiased estimator of the variance.
##
## @item 1:
## Normalize with @math{N}. This provides the square root of the second moment
## around the mean.
##
## @item a vector:
## Compute the weighted standard deviation with nonnegative scalar weights. The
## length of @var{w} must be equal to the size of @var{x} along dimension
## @var{dim}.
## @end table
##
## If @math{N} is equal to 1 the value of @var{W} is ignored and
## normalization by @math{N} is used.
##
## The optional variable @var{dim} forces @code{std} to operate over the
## specified dimension.  @var{dim} can either be a scalar dimension or a vector
## of non-repeating dimensions over which to operate.  Dimensions must be
## positive integers, and the standard deviation is calculated over the array
## slice defined by @var{dim}.
##
## Specifying dimension @qcode{"ALL"} will force @code{std} to operate on all
## elements of @var{x}, and is equivalent to @code{std (@var{x}(:))}.
##
## When @var{dim} is a vector or @qcode{"ALL"}, @var{w} must be either 0 or 1.
## @seealso{var, bounds, mad, range, iqr, mean, median}
## @end deftypefn

function retval = std (varargin)

  retval = sqrt (var (varargin{:}));

endfunction


%!test
%! x = ones (10, 2);
%! y = [1, 3];
%! assert (std (x), [0, 0]);
%! assert (std (y), sqrt (2), sqrt (eps));
%! assert (std (x, 0, 2), zeros (10, 1));

%!assert (std (ones (3, 1, 2), 0, 2), zeros (3, 1, 2))
%!assert (std ([1 2], 0), sqrt (2)/2, 5*eps)
%!assert (std ([1 2], 1), 0.5, 5*eps)
%!assert (std (1), 0)
%!assert (std (single (1)), single (0))
%!assert (std ([1 2 3], [], 3), [0 0 0])

##Test empty inputs
%!assert (std ([]), NaN)
%!assert (std ([],[],1), NaN(1,0))
%!assert (std ([],[],2), NaN(0,1))
%!assert (std ([],[],3), [])
%!assert (std (ones (0,1)), NaN)
%!assert (std (ones (1,0)), NaN)
%!assert (std (ones (1,0), [], 1), NaN(1,0))
%!assert (std (ones (1,0), [], 2), NaN)
%!assert (std (ones (1,0), [], 3), NaN(1,0))
%!assert (std (ones (0,1)), NaN)
%!assert (std (ones (0,1), [], 1), NaN)
%!assert (std (ones (0,1), [], 2), NaN(0,1))
%!assert (std (ones (0,1), [], 3), NaN(0,1))
%!assert (std (ones (1,3,0,2)), NaN(1,1,0,2))
%!assert (std (ones (1,3,0,2), [], 1), NaN(1,3,0,2))
%!assert (std (ones (1,3,0,2), [], 2), NaN(1,1,0,2))
%!assert (std (ones (1,3,0,2), [], 3), NaN(1,3,1,2))
%!assert (std (ones (1,3,0,2), [], 4), NaN(1,3,0))


## Test input validation
%!error <Invalid call> std ()
%!error <X must be a numeric> std (['A'; 'B'])
%!error <W must be 0> std ([1 2], 2)
%!error <DIM must be a positive integer> std (1, [], ones (2,2))
%!error <DIM must be a positive integer> std (1, [], 1.5)
%!error <DIM must be a positive integer> std (1, [], 0)
