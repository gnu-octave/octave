########################################################################
##
## Copyright (C) 1996-2025 The Octave Project Developers
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
## @deftypefn  {} {@var{y} =} skewness (@var{x})
## @deftypefnx {} {@var{y} =} skewness (@var{x}, @var{flag})
## @deftypefnx {} {@var{y} =} skewness (@var{x}, @var{flag}, @var{dim})
## @deftypefnx {} {@var{y} =} skewness (@var{x}, @var{flag}, @var{vecdim})
## @deftypefnx {} {@var{y} =} skewness (@var{x}, @var{flag}, "all")
## Compute the sample skewness of the input data @var{x}.
##
## The sample skewness is defined as
## @tex
## $$
## {\rm skewness} (@var{x}) = {{{1\over N}\,
##          \sum_{i=1}^N (x_i - \bar{x})^3} \over \sigma^3},
## $$
## where $N$ is the length of @var{x}, $\bar{x}$ its mean and $\sigma$
## its (uncorrected) standard deviation.
## @end tex
## @ifnottex
##
## @example
## @group
##                mean ((@var{x} - mean (@var{x})).^3)
## skewness (@var{X}) = ------------------------.
##                       std (@var{x}).^3
## @end group
## @end example
##
## @end ifnottex
##
## @noindent
## The optional argument @var{flag} controls which normalization is used.
## If @var{flag} is equal to 1 (default value, used when @var{flag} is omitted
## or empty), return the sample skewness as defined above.  If @var{flag} is
## equal to 0, return the adjusted skewness coefficient instead:
## @tex
## $$
## {\rm skewness} (@var{x}) = {\sqrt{N (N - 1)} \over N - 2} \times \,
##   {{{1 \over N} \sum_{i=1}^N (x_i - \bar{x})^3} \over \sigma^3}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##                   sqrt (N*(N-1))   mean ((@var{x} - mean (@var{x})).^3)
## skewness (@var{X}, 0) = -------------- * ------------------------.
##                       (N - 2)             std (@var{x}).^3
## @end group
## @end example
##
## @noindent
## where @math{N} is the length of the @var{x} vector.
##
## @end ifnottex
## The adjusted skewness coefficient is obtained by replacing the sample second
## and third central moments by their bias-corrected versions.
##
## If @var{x} is a vector, then @code{skewness (@var{x})} computes the skewness
## of the data in @var{x}.
##
## If @var{x} is a matrix, then @code{skewness (@var{x})} returns a row vector
## with each element containing the skewness of the data of the corresponding
## column in @var{x}.
##
## If @var{x} is an array, then @code{skewness (@var{x})} computes the skewness
## of the data along the first non-singleton dimension of @var{x}.
##
## The data in @var{x} must be numeric and any NaN values are ignored.
## The size of @var{y} is equal to the size of @var{x} except for the operating
## dimension, which becomes 1.
##
## The optional input @var{dim} specifies the dimension to operate on and must
## be a positive integer.  Specifying any singleton dimension of @var{x},
## including any dimension exceeding @code{ndims (@var{x})}, will return
## @code{@var{x}}.
##
## Specifying the dimensions as @var{vecdim}, a vector of non-repeating
## dimensions, will return the mean square over the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim}
## greater than @code{ndims (@var{x})} is ignored.
##
## Specifying the dimension as @qcode{"all"} will cause @code{meansq} to operate
## on all elements of @var{x}, and is equivalent to @code{meansq (@var{x}(:))}.
## @seealso{var, kurtosis, moment}
## @end deftypefn

function y = skewness (x, flag, dim)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x)))
    error ("skewness: X must be a numeric array");
  endif

  if (nargin < 2 || isempty (flag))
    flag = 1;  # default: do not use the "bias corrected" version
  elseif (! isscalar (flag) || (flag != 0 && flag != 1))
    error ("skewness: FLAG must be 0 or 1");
  endif

  if (nargin < 3)
    (dim = find (size (x) != 1, 1)) || (dim = 1);
  endif

  n = sum (x == x, dim, "omitnan");

  x = center (x, dim, "omitnan");
  s = std (x, 1, dim, "omitnan");   # normalize with 1/N
  y = sum (x .^ 3, dim, "omitnan");
  y = y ./ (n .* s .^ 3);
  idx = (s != 0);
  y(! idx) = NaN;

  ## Apply bias correction to the second and third central sample moment
  if (flag == 0)
    idx = n > 2;
    nn = n(idx);
    y(idx) = y(idx) .* sqrt (nn .* (nn - 1)) ./ (nn - 2);
    y(! idx) = NaN;
  endif

endfunction


%!assert (skewness ([-1, 0, 1]), 0)
%!assert (skewness ([-2, 0, 1]) < 0)
%!assert (skewness ([-1, 0, 2]) > 0)
%!assert (skewness ([-3, 0, 1]) == -1 * skewness ([-1, 0, 3]))
%!assert (skewness (ones (3, 5)), NaN (1, 5))
%!assert (skewness (1, [], 3), NaN)

%!test
%! x = [0; 0; 0; 1];
%! y = [x, 2*x];
%! assert (skewness (y), 1.154700538379251 * [1 1], 5*eps);

%!assert (skewness ([1:5 10; 1:5 10],  0, 2), 1.439590274527954 * [1; 1], eps)
%!assert (skewness ([1:5 10; 1:5 10],  1, 2), 1.051328089232020 * [1; 1], 2*eps)
%!assert (skewness ([1:5 10; 1:5 10], [], 2), 1.051328089232020 * [1; 1], 2*eps)

## Test behavior on single input
%!assert (skewness (single ([1:5 10])), single (1.0513283), eps ("single"))
%!assert (skewness (single ([1 2]), 0), single (NaN))

## Test dim
%!test
%! x(:,:,1) = [0.5377, 0.3188, 3.5784; 1.8339, -1.3077, 2.7694; ...
%!             -2.2588, -0.4336, -1.3499; 0.8622, 0.3426, 3.0349];
%! x(:,:,2) = [0.7254, -0.1241, 0.6715; -0.0631, 1.4897, -1.2075; ...
%!             0.7147 1.4090 0.7172; -0.2050, 1.4172, 1.6302];
%! y = skewness (x);
%! assert (y(:,:,1), [-0.8084, -0.5578, -1.0772], 1e-4);
%! assert (y(:,:,2), [-0.0403, -1.1472, -0.6632], 1e-4);
%! y = skewness (x, 1, 2);
%! assert (y(:,:,1), [0.6956; -0.5575; 0.0049; 0.6033], 1e-4);
%! assert (y(:,:,2), [-0.6969; 0.1828; 0.7071; -0.6714], 1e-4);
%! y = skewness (x, 1, 3);
%! assert (y, zeros (4, 3), 8 * eps);

## Test "all" and vecdim
%!test
%! x(:,:,1) = [0.5377, 0.3188, 3.5784; 1.8339, -1.3077, 2.7694; ...
%!             -2.2588, -0.4336, -1.3499; 0.8622, 0.3426, 3.0349];
%! x(:,:,2) = [0.7254, -0.1241, 0.6715; -0.0631, 1.4897, -1.2075; ...
%!             0.7147 1.4090 0.7172; -0.2050, 1.4172, 1.6302];
%! y = skewness (x, 1, "all");
%! assert (y, 0.0916, 1e-4);
%! y = skewness (x, 1, [1, 2]);
%! assert (y(:,:,1), 0.1070, 1e-4);
%! assert (y(:,:,2), -0.6263, 1e-4);
%! y = skewness (x, 1, [1, 3]);
%! assert (y, [-1.0755, -0.3108, -0.2209], 1e-4);

## Verify no warnings
%!test
%! lastwarn ("");  # clear last warning
%! skewness (1);
%! assert (lastwarn (), "");

## Test input validation
%!error <Invalid call> skewness ()
%!error <skewness: X must be a numeric array> skewness (['A'; 'B'])
%!error <skewness: X must be a numeric array> skewness ([true; false])
%!error <skewness: X must be a numeric array> skewness ({1, 2, 3})
%!error <skewness: FLAG must be 0 or 1> skewness (1, 2)
%!error <skewness: FLAG must be 0 or 1> skewness (1, [1 0])
