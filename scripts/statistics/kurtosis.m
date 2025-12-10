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
## @deftypefn  {} {@var{y} =} kurtosis (@var{x})
## @deftypefnx {} {@var{y} =} kurtosis (@var{x}, @var{flag})
## @deftypefnx {} {@var{y} =} kurtosis (@var{x}, @var{flag}, @var{dim})
## @deftypefnx {} {@var{y} =} kurtosis (@var{x}, @var{flag}, @var{vecdim})
## @deftypefnx {} {@var{y} =} kurtosis (@var{x}, @var{flag}, "all")
## Compute the sample kurtosis of the input data @var{x}.
##
## The sample kurtosis is defined as
## @tex
## $$
## \kappa_1 = {{{1\over N}\,
##          \sum_{i=1}^N (x_i - \bar{x})^4} \over \sigma^4},
## $$
## where $N$ is the length of @var{x}, $\bar{x}$ its mean, and $\sigma$
## its (uncorrected) standard deviation.
## @end tex
## @ifnottex
##
## @example
## @group
##      mean ((@var{x} - mean (@var{x})).^4)
## k1 = ------------------------
##             std (@var{x}).^4
## @end group
## @end example
##
## @end ifnottex
##
## @noindent
## The optional argument @var{flag} controls which normalization is used.
## If @var{flag} is equal to 1 (default value, used when @var{flag} is omitted
## or empty), return the sample kurtosis as defined above.  If @var{flag} is
## equal to 0, return the @w{"bias-corrected"}@ kurtosis coefficient instead:
## @tex
## $$
## \kappa_0 = 3 + {\scriptstyle N - 1 \over \scriptstyle (N - 2)(N - 3)} \,
##     \left( (N + 1)\, \kappa_1 - 3 (N - 1) \right)
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##               N - 1
## k0 = 3 + -------------- * ((N + 1) * k1 - 3 * (N - 1))
##          (N - 2)(N - 3)
## @end group
## @end example
##
## @noindent
## where @math{N} is the length of the @var{x} vector.
##
## @end ifnottex
## The bias-corrected kurtosis coefficient is obtained by replacing the sample
## second and fourth central moments by their unbiased versions.  It is an
## unbiased estimate of the population kurtosis for normal populations.
##
## If @var{x} is a vector, then @code{kurtosis (@var{x})} computes the kurtosis
## of the data in @var{x}.
##
## If @var{x} is a matrix, then @code{kurtosis (@var{x})} returns a row vector
## with each element containing the kurtosis of the data of the corresponding
## column in @var{x}.
##
## If @var{x} is an array, then @code{kurtosis (@var{x})} computes the kurtosis
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
##
## @seealso{var, skewness, moment}
## @end deftypefn

function y = kurtosis (x, flag, dim)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x)))
    error ("kurtosis: X must be a numeric array");
  endif

  if (nargin < 2 || isempty (flag))
    flag = 1;  # default: do not use the "bias corrected" version
  else
    if (! isscalar (flag) || (flag != 0 && flag != 1))
      error ("kurtosis: FLAG must be 0 or 1");
    endif
  endif

  if (nargin < 3)
    (dim = find (size (x) != 1, 1)) || (dim = 1);
  endif

  n = sum (x == x, dim, "omitnan");

  x = center (x, dim, "omitnan");
  v = var (x, 1, dim, "omitnan");   # normalize with 1/N
  y = sum (x .^ 4, dim, "omitnan");
  y = y ./ (n .* v .^ 2);
  idx = (v != 0);
  y(! idx) = NaN;

  ## Apply bias correction to the second and fourth central sample moment
  if (flag == 0)
    idx = n > 3;
    nn = n(idx);
    C = (nn - 1) ./ ((nn - 2) .* (nn - 3));
    y(idx) = 3 + C .* ((nn + 1) .* y(idx) - 3 .* (nn - 1));
    y(! idx) = NaN;
  endif

endfunction


%!test
%! x = [-1; 0; 0; 0; 1];
%! y = [x, 2*x];
%! assert (kurtosis (y), [2.5, 2.5], sqrt (eps));

%!assert (kurtosis ([-3, 0, 1]) == kurtosis ([-1, 0, 3]))
%!assert (kurtosis (ones (3, 5)), NaN (1, 5))
%!assert (kurtosis (1, [], 3), NaN)

%!assert (kurtosis ([1:5 10; 1:5 10],  0, 2),
%!        5.4377317925288901 * [1; 1], 8 * eps)
%!assert (kurtosis ([1:5 10; 1:5 10],  1, 2),
%!        2.9786509002956195 * [1; 1], 8 * eps)
%!assert (kurtosis ([1:5 10; 1:5 10], [], 2),
%!        2.9786509002956195 * [1; 1], 8 * eps)

## Test behavior on single input
%!assert (kurtosis (single ([1:5 10])), single (2.9786513), eps ("single"))
%!assert (kurtosis (single ([1 2]), 0), single (NaN))

## Test dim
%!test
%! x(:,:,1) = [0.5377, 0.3188, 3.5784; 1.8339, -1.3077, 2.7694; ...
%!             -2.2588, -0.4336, -1.3499; 0.8622, 0.3426, 3.0349];
%! x(:,:,2) = [0.7254, -0.1241, 0.6715; -0.0631, 1.4897, -1.2075; ...
%!             0.7147 1.4090 0.7172; -0.2050, 1.4172, 1.6302];
%! y = kurtosis (x);
%! assert (y(:,:,1), [2.1350, 1.7060, 2.2789], 1e-4);
%! assert (y(:,:,2), [1.0542, 2.3278, 2.0996], 1e-4);
%! y = kurtosis (x, 1, 2);
%! assert (y(:,:,1), [1.5; 1.5; 1.5; 1.5], 8 * eps);
%! assert (y(:,:,2), [1.5; 1.5; 1.5; 1.5], 8 * eps);
%! y = kurtosis (x, 1, 3);
%! assert (y, ones (4, 3), 8 * eps);

## Test "all" and vecdim
%!test
%! x(:,:,1) = [0.5377, 0.3188, 3.5784; 1.8339, -1.3077, 2.7694; ...
%!             -2.2588, -0.4336, -1.3499; 0.8622, 0.3426, 3.0349];
%! x(:,:,2) = [0.7254, -0.1241, 0.6715; -0.0631, 1.4897, -1.2075; ...
%!             0.7147 1.4090 0.7172; -0.2050, 1.4172, 1.6302];
%! y = kurtosis (x, 1, "all");
%! assert (y, 2.8029, 1e-4);
%! y = kurtosis (x, 1, [1, 2]);
%! assert (y(:,:,1), 1.9345, 1e-4);
%! assert (y(:,:,2), 2.5877, 1e-4);
%! y = kurtosis (x, 1, [2, 3]);
%! assert (y, [3.8457; 1.4306; 1.7094; 2.3378], 1e-4);

## Verify no warnings
%!test
%! lastwarn ("");  # clear last warning
%! kurtosis (1);
%! assert (lastwarn (), "");

## Test input validation
%!error <Invalid call> kurtosis ()
%!error <kurtosis: function called with too many inputs> kurtosis (1, 2, 3, 4)
%!error <kurtosis: X must be a numeric array> kurtosis (['A'; 'B'])
%!error <kurtosis: X must be a numeric array> kurtosis ([true; false])
%!error <kurtosis: X must be a numeric array> kurtosis ({1, 2})
%!error <FLAG must be 0 or 1> kurtosis (1, 2)
%!error <FLAG must be 0 or 1> kurtosis (1, [1 0])
