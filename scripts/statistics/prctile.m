########################################################################
##
## Copyright (C) 2008-2025 The Octave Project Developers
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
## @deftypefn  {} {@var{q} =} prctile (@var{x})
## @deftypefnx {} {@var{q} =} prctile (@var{x}, @var{p})
## @deftypefnx {} {@var{q} =} prctile (@var{x}, @var{p}, @var{dim})
## @deftypefnx {} {@var{q} =} prctile (@var{x}, @var{p}, @var{vecdim})
## @deftypefnx {} {@var{q} =} prctile (@var{x}, @var{p}, "all")
## @deftypefnx {} {@var{q} =} prctile (@var{x}, @var{p}, @dots{}, @var{method})
## Compute the percentiles of the input data @var{x}.
##
## If @var{x} is a vector, then @code{prctile (@var{x})} computes the
## percentiles specified by @var{p} of the data in @var{x}.
##
## If @var{x} is a matrix, then @code{prctile (@var{x})} returns a matrix such
## that the i-th row of @var{q} contains the @var{p}(i)th percentiles of each
## column of @var{x}.
##
## If @var{x} is an array, then @code{prctile (@var{x})} computes the
## percentiles specified by @var{p} along the first non-singleton dimension of
## @var{x}.
##
## The data in @var{x} must be numeric and any NaN values are ignored.  The size
## of @var{q} is equal to the size of @var{x} except for the operating
## dimension, which equals to the number of quantiles specified by @var{p}.
##
## @var{p} is a numeric vector specifying the percentiles to be computed.  All
## elements of @var{p} must be in the range from 0 to 100.  If @var{p} is
## unspecified, return the percentiles for @code{[0 25 50 75 100]}.
##
## The optional input @var{dim} specifies the dimension to operate on and must
## be a positive integer.  Specifying any singleton dimension of @var{x},
## including any dimension exceeding @code{ndims (@var{x})}, will return N
## copies of @var{x} along the operating dimension, where N is the number of
## specified percentiles.
##
## Specifying multiple dimensions with input @var{vecdim}, a vector of
## non-repeating dimensions, will operate along the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim}
## greater than @code{ndims (@var{x})} is ignored.  If all dimensions in
## @var{vecdim} are greater than @code{ndims (@var{x})}, then @code{quantile}
## will return N copies of @var{x} along the smallest dimension in @var{vecdim}.
##
## Specifying the dimension as @qcode{"all"} will cause @code{iqr} to operate
## on all elements of @var{x}, and is equivalent to @code{iqr (@var{x}(:))}.
##
## The fourth input argument, @var{methods}, determines the method to calculate
## the percentiles specified by @var{p}.  The methods available to calculate
## sample percentiles are the nine methods used by R
## (@url{https://www.r-project.org/}) and can be specified by the corresponding
## integer value.  The default value is @w{@var{method} = 5}.
##
## Discontinuous sample quantile methods 1, 2, and 3
##
## @enumerate 1
## @item Method 1: Inverse of empirical distribution function.
##
## @item Method 2: Similar to method 1 but with averaging at discontinuities.
##
## @item Method 3: SAS definition: nearest even order statistic.
## @end enumerate
##
## Continuous sample quantile methods 4 through 9, where
## @tex
## $p(k)$
## @end tex
## @ifnottex
## @var{p}(k)
## @end ifnottex
## is the linear
## interpolation function respecting each method's representative cdf.
##
## @enumerate 4
## @item Method 4:
## @tex
## $p(k) = k / N$.
## @end tex
## @ifnottex
## @var{p}(k) = k / N.
## @end ifnottex
## That is, linear interpolation of the empirical cdf, where @math{N} is the
## length of @var{P}.
##
## @item Method 5:
## @tex
## $p(k) = (k - 0.5) / N$.
## @end tex
## @ifnottex
## @var{p}(k) = (k - 0.5) / N.
## @end ifnottex
## That is, a piecewise linear function where the knots are the values midway
## through the steps of the empirical cdf.
##
## @item Method 6:
## @tex
## $p(k) = k / (N + 1)$.
## @end tex
## @ifnottex
## @var{p}(k) = k / (N + 1).
## @end ifnottex
##
## @item Method 7:
## @tex
## $p(k) = (k - 1) / (N - 1)$.
## @end tex
## @ifnottex
## @var{p}(k) = (k - 1) / (N - 1).
## @end ifnottex
##
## @item Method 8:
## @tex
## $p(k) = (k - 1/3) / (N + 1/3)$.
## @end tex
## @ifnottex
## @var{p}(k) = (k - 1/3) / (N + 1/3).
## @end ifnottex
## The resulting quantile estimates are approximately median-unbiased
## regardless of the distribution of @var{x}.
##
## @item Method 9:
## @tex
## $p(k) = (k - 3/8) / (N + 1/4)$.
## @end tex
## @ifnottex
## @var{p}(k) = (k - 3/8) / (N + 1/4).
## @end ifnottex
## The resulting quantile estimates are approximately unbiased for the
## expected order statistics if @var{x} is normally distributed.
## @end enumerate
## @seealso{quantile}
## @end deftypefn

function q = prctile (x, p = [], dim, method)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x)))
    error ("prctile: X must be a numeric array");
  endif

  if (isempty (p))
    p = [0, 25, 50, 75, 100];
  endif

  if (! (isnumeric (p) && isvector (p)))
    error ("prctile: P must be a numeric vector");
  endif

  if (any (p < 0 | p > 100))
    error ("prctile: P values must range from 0 to 100");
  endif

  ## Convert from percent to decimal.
  p /= 100;

  if (nargin < 3)
    q = quantile (x, p);
  elseif (nargin < 4)
    q = quantile (x, p, dim);
  elseif (nargin < 5)
    q = quantile (x, p, dim, method);
  endif

endfunction


%!test
%! pct = 50;
%! q = prctile (1:4, pct);
%! qa = 2.5;
%! assert (q, qa);
%! q = prctile (1:4, pct, 1);
%! qa = [1, 2, 3, 4];
%! assert (q, qa);
%! q = prctile (1:4, pct, 2);
%! qa = 2.5;
%! assert (q, qa);

%!test
%! pct = [50 75];
%! q = prctile (1:4, pct);
%! qa = [2.5 3.5];
%! assert (q, qa);
%! q = prctile (1:4, pct, 1);
%! qa = [1, 2, 3, 4; 1, 2, 3, 4];
%! assert (q, qa);
%! q = prctile (1:4, pct, 2);
%! qa = [2.5 3.5];
%! assert (q, qa);

%!test
%! pct = 50;
%! x = [0.1126, 0.1148, 0.0521, 0.2364, 0.1393
%!      0.1718, 0.7273, 0.2041, 0.4531, 0.1585
%!      0.2795, 0.7978, 0.3296, 0.5567, 0.7307
%!      0.4288, 0.8753, 0.6477, 0.6287, 0.8165
%!      0.9331, 0.9312, 0.9635, 0.7796, 0.8461];
%! tol = 0.0001;
%! q = prctile (x, pct, 1);
%! qa = [0.2795, 0.7978, 0.3296, 0.5567, 0.7307];
%! assert (q, qa, tol);
%! q = prctile (x, pct, 2);
%! qa = [0.1148; 0.2041; 0.5567; 0.6477; 0.9312];
%! assert (q, qa, tol);

%!test
%! pct = 50;
%! tol = 0.0001;
%! x = [0.1126, 0.1148, 0.0521, 0.2364, 0.1393
%!      0.1718, 0.7273, 0.2041, 0.4531, 0.1585
%!      0.2795, 0.7978, 0.3296, 0.5567, 0.7307
%!      0.4288, 0.8753, 0.6477, 0.6287, 0.8165
%!      0.9331, 0.9312, 0.9635, 0.7796, 0.8461];
%! x(5,5) = Inf;
%! q = prctile (x, pct, 1);
%! qa = [0.2795, 0.7978, 0.3296, 0.5567, 0.7307];
%! assert (q, qa, tol);
%! x(5,5) = -Inf;
%! q = prctile (x, pct, 1);
%! qa = [0.2795, 0.7978, 0.3296, 0.5567, 0.1585];
%! assert (q, qa, tol);
%! x(1,1) = Inf;
%! q = prctile (x, pct, 1);
%! qa = [0.4288, 0.7978, 0.3296, 0.5567, 0.1585];
%! assert (q, qa, tol);

%!test
%! pct = 50;
%! tol = 0.0001;
%! x = [0.1126, 0.1148, 0.0521, 0.2364, 0.1393
%!      0.1718, 0.7273, 0.2041, 0.4531, 0.1585
%!      0.2795, 0.7978, 0.3296, 0.5567, 0.7307
%!      0.4288, 0.8753, 0.6477, 0.6287, 0.8165
%!      0.9331, 0.9312, 0.9635, 0.7796, 0.8461];
%! x(3,3) = Inf;
%! q = prctile (x, pct, 1);
%! qa = [0.2795, 0.7978, 0.6477, 0.5567, 0.7307];
%! assert (q, qa, tol);
%! q = prctile (x, pct, 2);
%! qa = [0.1148; 0.2041; 0.7307; 0.6477; 0.9312];
%! assert (q, qa, tol);

%!test
%! pct = 50;
%! tol = 0.0001;
%! x = [0.1126, 0.1148, 0.0521, 0.2364, 0.1393
%!      0.1718, 0.7273, 0.2041, 0.4531, 0.1585
%!      0.2795, 0.7978, 0.3296, 0.5567, 0.7307
%!      0.4288, 0.8753, 0.6477, 0.6287, 0.8165
%!      0.9331, 0.9312, 0.9635, 0.7796, 0.8461];
%! x(5,5) = NaN;
%! q = prctile (x, pct, 2);
%! qa = [0.1148; 0.2041; 0.5567; 0.6477; 0.9322];
%! assert (q, qa, tol);
%! x(1,1) = NaN;
%! q = prctile (x, pct, 2);
%! qa = [0.1270; 0.2041; 0.5567; 0.6477; 0.9322];
%! assert (q, qa, tol);
%! x(3,3) = NaN;
%! q = prctile (x, pct, 2);
%! qa = [0.1270; 0.2041; 0.6437; 0.6477; 0.9322];
%! assert (q, qa, tol);

%!assert (prctile ([1:10], 1, 3), [1:10])

## Test input validation
%!error <Invalid call> prctile ()
%!error <prctile: X must be a numeric array> prctile (['A'; 'B'], 10)
%!error <prctile: X must be a numeric array> prctile ([true; false], 10)
%!error <prctile: X must be a numeric array> prctile ({1, 2, 3}, 10)
%!error <prctile: P must be a numeric vector> prctile (1:10, [true, false])
%!error <prctile: P must be a numeric vector> prctile (1:10, ones (2, 3))
%!error <prctile: P values must range from 0 to 100> prctile (1:10, -20)
