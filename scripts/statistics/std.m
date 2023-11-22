########################################################################
##
## Copyright (C) 1996-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{s} =} std (@var{x})
## @deftypefnx {} {@var{s} =} std (@var{x}, @var{w})
## @deftypefnx {} {@var{s} =} std (@var{x}, @var{w}, @var{dim})
## @deftypefnx {} {@var{s} =} std (@var{x}, @var{w}, @var{vecdim})
## @deftypefnx {} {@var{s} =} std (@var{x}, @var{w}, @qcode{"ALL"})
## @deftypefnx {} {@var{s} =} std (@dots{}, @var{nanflag})
## @deftypefnx {} {[@var{s}, @var{m}] =} std (@dots{})
## Compute the standard deviation of the elements of the vector @var{x}.
##
## The standard deviation is defined as
## @tex
## $$ {\rm std}(x) = \sqrt{{1\over N-1} \sum_{i=1}^N (x_i - \bar x )^2} $$
## where $\bar{x}$ is the mean value of @var{x} and $N$ is the number of
## elements of @var{x}.
## @end tex
## @ifnottex
##
## @example
## @group
## std (@var{x}) = sqrt ((1 / (N-1)) * SUM_i ((@var{x}(i) - mean(@var{x}))^2))
## @end group
## @end example
##
## @noindent
## where @math{N} is the number of elements of @var{x}.
## @end ifnottex
##
## If @var{x} is an array, compute the standard deviation along the first
## non-singleton dimensions of @var{x}.
##
## The optional argument @var{w} determines the weighting scheme to use.  Valid
## values are:
##
## @table @asis
## @item 0 [default]:
## Normalize with @math{N-1} (population standard deviation).  This provides
## the square root of the best unbiased estimator of the standard deviation.
##
## @item 1:
## Normalize with @math{N} (sample standard deviation).  This provides the
## square root of the second moment around the mean.
##
## @item a vector:
## Compute the weighted standard deviation with non-negative weights.
## The length of @var{w} must equal the size of @var{x} in the operating
## dimension.  NaN values are permitted in @var{w}, will be multiplied with the
## associated values in @var{x}, and can be excluded by the @var{nanflag}
## option.
##
## @item an array:
## Similar to vector weights, but @var{w} must be the same size as @var{x}.  If
## the operating dimension is supplied as @var{vecdim} or @qcode{"all"} and
## @var{w} is not a scalar, @var{w} must be an same-sized array.
## @end table
##
## Note: @var{w} must always be specified before specifying any of the
## following dimension options.  To use the default value for @var{w} you
## may pass an empty input argument [].
##
## The optional variable @var{dim} forces @code{std} to operate over the
## specified dimension, which must be a positive integer-valued number.
## Specifying any singleton dimension in @var{x}, including any dimension
## exceeding @code{ndims (@var{x})}, will result in a standard deviation of 0.
##
## Specifying the dimensions as  @var{vecdim}, a vector of non-repeating
## dimensions, will return the standard deviation calculated over the array
## slice defined by @var{vecdim}.  If @var{vecdim} indexes all dimensions of
## @var{x}, then it is equivalent to the option @qcode{"all"}.  Any
## dimension in @var{vecdim} greater than @code{ndims (@var{x})} is ignored.
##
## Specifying the dimension as @qcode{"all"} will force @code{std} to
## operate on all elements of @var{x}, and is equivalent to
## @code{std (@var{x}(:))}.
##
## The optional variable @var{nanflag} specifies whether to include or exclude
## NaN values from the calculation using any of the previously specified input
## argument combinations.  The default value for @var{nanflag} is
## @qcode{"includenan"} which keeps NaN values in the calculation.  To
## exclude NaN values set the value of @var{nanflag} to @qcode{"omitnan"}.
## The output will still contain NaN values if @var{x} consists of all NaN
## values in the operating dimension.
##
## The optional second output variable @var{m} contains the mean of the
## elements of @var{x} used to calculate the standard deviation.  If @var{v} is
## the weighted standard deviation, then @var{m} is also the weighted mean.
##
## @seealso{var, bounds, mad, range, iqr, mean, median}
## @end deftypefn

function [s, m] = std (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargout < 2)
    s = sqrt (var (varargin{:}));
  else
    [s, m] = var (varargin{:});
    s = sqrt (s);
  endif

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

## Test input validation
%!error <Invalid call> std ()
