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
## @deftypefn  {} {@var{p} =} polyfit (@var{x}, @var{y}, @var{n})
## @deftypefnx {} {[@var{p}, @var{s}] =} polyfit (@var{x}, @var{y}, @var{n})
## @deftypefnx {} {[@var{p}, @var{s}, @var{mu}] =} polyfit (@var{x}, @var{y}, @var{n})
## Return the coefficients of a polynomial @var{p}(@var{x}) of degree @var{n}
## that minimizes the least-squares-error of the fit to the points
## @code{[@var{x}(:), @var{y}(:)]}.
##
## @var{n} is typically an integer @geq{} 0 specifying the degree of the
## approximating polynomial.  If @var{n} is a logical vector, it is used as a
## mask to selectively force the corresponding polynomial coefficients to be
## used or ignored.
##
## The polynomial coefficients are returned in the row vector @var{p}.  The
## output @var{p} may be directly used with @code{polyval} to estimate values
## using the fitted polynomial.
##
## The optional output @var{s} is a structure containing the following fields:
##
## @table @samp
##
## @item yf
## The values of the polynomial for each value of @var{x}.
##
## @item V
## The @nospell{Vandermonde} matrix used to compute the polynomial
## coefficients.
##
## @item X (deprecated, will be removed in Octave 13)
## The @nospell{Vandermonde} matrix used to compute the polynomial
## coefficients.
##
## @item R
## Triangular factor R from the QR@tie{}decomposition.
##
## @item C
## The unscaled covariance matrix, formally equal to the inverse of
## @var{v'}*@var{v}, but computed in a way minimizing roundoff error
## propagation.
##
## @item df
## The degrees of freedom.
##
## @item normr
## The norm of the residuals.
##
## @item rsquared
## Coefficient of determination, or R-squared.
## @end table
##
## The second output may be used by @code{polyval} to calculate the statistical
## error limits of the predicted values.  In particular, the standard deviation
## of @var{p} coefficients is given by
##
## @code{sqrt (diag (@var{s.C})/@var{s.df}) * @var{s.normr}}.
##
## When the third output, @var{mu}, is present the original data is centered
## and scaled which can improve the numerical stability of the fit.  The
## coefficients @var{p} are associated with a polynomial in
##
## @code{@var{xhat} = (@var{x} - @var{mu}(1)) / @var{mu}(2)} @*
## where @var{mu}(1) = mean (@var{x}), and @var{mu}(2) = std (@var{x}).
##
## Example 1 : logical @var{n} and integer @var{n}
##
## @example
## @group
## f = @@(x) x.^2 + 5;   # data-generating function
## x = 0:5;
## y = f (x);
## ## Fit data to polynomial A*x^3 + B*x^1
## p = polyfit (x, y, logical ([1, 0, 1, 0]))
## @result{} p = [ 0.0680, 0, 4.2444, 0 ]
## ## Fit data to polynomial using all terms up to x^3
## p = polyfit (x, y, 3)
## @result{} p = [ -4.9608e-17, 1.0000e+00, -1.6906e-15, 5.0000e+00 ]
## @end group
## @end example
##
## Programming Note: If the desired polynomial degree @var{n} @geq{} number
## of data points, the solution is not unique.  Instead, the fitting calculates
## @code{@var{m} = numel (@var{x}) - 1} terms.  The return polynomial has
## coefficients for @var{x}^@var{n}, @var{x}^@var{n}-1, @dots{},
## @var{x}^@var{n}-@var{m}, and the constant term @var{x}^0, with all other
## coefficients set to 0.
## @seealso{polyval, polyaffine, roots, vander, zscore}
## @end deftypefn

function [p, s, mu] = polyfit (x, y, n)

  if (nargin < 3)
    print_usage ();
  endif

  scale_x = (nargout == 3);
  y_is_row_vector = isrow (y);
  
  ## Reshape x and y into column vectors.
  x = x(:);
  y = y(:);

  nx = numel (x);
  ny = numel (y);
  if (nx != ny)
    error ("polyfit: X and Y must have the same number of points");
  endif

  if (scale_x)
    ## Center and scale the x values.
    mu = [mean(x), std(x)];
    x = (x - mu(1)) / mu(2);
  endif

  ## n is the polynomial degree (an input, or deduced from the polymask size).
  ## m is the effective number of coefficients (accounts for insufficient data).
  if (islogical (n))
    polymask = n(:).';          # force to row vector
    n = numel (polymask) - 1;
    m = sum (polymask) - 1;
    pad_output = true;
  else
    if (! (isscalar (n) && n >= 0 && ! isinf (n) && n == fix (n)))
      error ("polyfit: N must be a non-negative integer");
    endif
    polymask = true (1, n+1);
    m = n;
    pad_output = false;
  endif

  if (m >= nx)
    warning ("polyfit: degree of polynomial N is >= number of data points; solution is not unique.");
    m = nx;
    pad_output = true;
    if (scale_x && nx < 3)
      error ("polyfit: number of data points must be > 2 when scaling input X");
    endif
    ## Keep the highest m-1 entries and the constant term in polymask
    polymask(:) = false;
    if (m == 1)
      polymask(1) = true;
    else
      polymask([1:m-1,end]) = true;
    endif
  endif

  ## Construct the Vandermonde matrix.
  v = vander (x, n+1);
  ## Reduce columns of matrix, rather than zero them, to improve qr results.
  v = v(:, polymask);  

  ## Solve by QR decomposition.
  [q, r, k] = qr (v, 0);  # Use '0' to create 'econ' factoring.
  p = r \ (q' * y);
  p(k) = p;

  if (nargout > 1)
    yf = v*p;
    if (y_is_row_vector)
      s.yf = yf.';
    else
      s.yf = yf;
    endif

    ## r.'*r is positive definite if matrix v is of full rank.  Invert it by
    ## cholinv to avoid taking the square root of squared quantities.
    ## If cholinv fails, then v is rank deficient and not invertible.
    try
      C = cholinv (r.'*r)(k, k);
    catch
      C = NaN (m, m);
    end_try_catch

    if (pad_output)
      s.V = zeros (rows (v), n+1); s.V(:, polymask) = v;
      ## FIXME: Deprecated.  Remove in Octave 13.
      s.X = s.V;
      s.R = zeros (rows (r), n+1); s.R(:, polymask) = r;
      s.C = zeros (rows (C), n+1); s.C(:, polymask) = C;
    else
      s.V = v;
      ## FIXME: Deprecated.  Remove in Octave 13.
      s.X = s.V;
      s.R = r;
      s.C = C;
    endif

    s.df = max (0, nx - m - 1);
    resid = sumsq (yf - y);
    s.normr = sqrt (resid);
    s.rsquared = 1 - resid / sumsq (y - mean (y));
  endif

  if (pad_output)
    ## Zero pad output
    p_nonzero = p;
    p = zeros (n+1, 1);
    p(polymask) = p_nonzero;
  endif
  p = p.';  # Return a row vector.

endfunction


%!shared x
%! x = [-2, -1, 0, 1, 2];

%!assert (polyfit (x, 3*x.^2 + 2*x + 1, 2), [3, 2, 1], 10*eps)
%!assert (polyfit (x, 3*x.^2 + 2*x + 1, logical ([1 1 1])), [3, 2, 1], 10*eps)
%!assert (polyfit (x, x.^2 + 2*x + 3, 3), [0, 1, 2, 3], 10*eps)
%!assert (polyfit (x, x.^2 + 2*x + 3, logical ([0 1 1 1])), [0 1 2 3], 10*eps)

## Test logical input N
%!test
%! x = [0:5];
%! y = 3*x.^3 + 2*x.^2 + 4;
%! [p, s] = polyfit (x, y, logical ([1, 0, 1, 1]));
%! assert (p(2), 0);
%! assert (all (p([1, 3, 4])));
%! assert (s.df, 3);

## Test difficult case where scaling is really needed.  This example
## demonstrates the rather poor result which occurs when the dependent
## variable is not normalized properly.
## Also check the usage of 2nd & 3rd output arguments.
%!test
%! warning ("off", "Octave:nearly-singular-matrix", "local");
%! x = [ -1196.4, -1195.2, -1194, -1192.8, -1191.6, -1190.4, -1189.2, -1188, ...
%!       -1186.8, -1185.6, -1184.4, -1183.2, -1182];
%! y = [ 315571.7086, 315575.9618, 315579.4195, 315582.6206, 315585.4966, ...
%!       315588.3172, 315590.9326, 315593.5934, 315596.0455, 315598.4201, ...
%!       315600.7143, 315602.9508, 315605.1765 ];
%! [p1, s1] = polyfit (x, y, 10);
%! [p2, s2, mu] = polyfit (x, y, 10);
%! assert (s2.normr < s1.normr);

%!test
%! warning ("off", "Octave:nearly-singular-matrix", "local");
%! x = 1000 + (-5:5);
%! xn = (x - mean (x)) / std (x);
%! pn = ones (1,5);
%! y = polyval (pn, xn);
%! n = numel (pn) - 1;
%! [p, s, mu] = polyfit (x, y, n);
%! [p2, s2] = polyfit (x, y, n);
%! assert (p, pn, s.normr);
%! assert (s.yf, y, s.normr);
%! assert (mu, [mean(x), std(x)]);
%! assert (s.normr/s2.normr < sqrt (eps));

## Complex polynomials
%!test
%! x = 1:4;
%! p0 = [1i, 0, 2i, 4];
%! y = polyval (p0, x);
%! n = numel (p0) - 1;
%! p = polyfit (x, y, n);
%! assert (p, p0, 1000*eps);

## Matrix input
%!test
%! x = [1, 2, 3; 4, 5, 6];
%! y = [0, 0, 1; 1, 0, 0];
%! p = polyfit (x, y, 5);
%! expected = [0, 1, -14, 65, -112, 60] / 12;
%! assert (p, expected, sqrt (eps));

## Orientation of output is always row vector
%!test
%! x = 0:5;
%! y = x.^4 + 2*x + 5;
%! [p, s] = polyfit (x, y, 3);
%! assert (isrow (s.yf));
%! [p, s] = polyfit (x, y.', 3);
%! assert (iscolumn (s.yf));

## Insufficient data for fit
%!test <*57964>
%! ## Disable warnings entirely because there is not a specific ID to disable.
%! wstate = warning ();
%! unwind_protect
%!   warning ("off", "all");
%!   x = [1, 2];
%!   y = [1.5, 9];
%!   [p,s] = polyfit (x, y, 4);
%! unwind_protect_cleanup
%!   warning (wstate);
%! end_unwind_protect
%! assert (p, [0.5, 0, 0, 0, 1], 12*eps);
%! assert (size (s.V), [2, 5]);
%! assert (s.V(:,2:4), zeros (2,3));
%! assert (size (s.R), [2, 5]);
%! assert (s.R(:,2:4), zeros (2,3));
%! assert (size (s.C), [2, 5]);
%! assert (s.C(:,2:4), zeros (2,3));
%! assert (s.df, 0);
%! assert (s.normr, 0, 20*eps);
%! assert (s.rsquared, 1, 20*eps);

%!test
%! wstate = warning ();
%! unwind_protect
%!   warning ("off", "all");
%!   x = [1, 2, 3];
%!   y = 2*x + 1;
%!   p0 = polyfit (x, y, logical ([1, 1, 1, 0, 1]));
%!   [p1, s, mu] = polyfit (x, y, logical ([1, 1, 1, 0, 1]));
%! unwind_protect_cleanup
%!   warning (wstate);
%! end_unwind_protect
%! assert (p0(3:4), [0, 0]);
%! assert (p1, [0, 2, 0, 0, 5], 10*eps);
%! assert (size (s.V), [3, 5]);
%! assert (s.V(:,3:4), zeros (3,2));
%! assert (size (s.R), [3, 5]);
%! assert (s.R(:,3:4), zeros (3,2));
%! assert (size (s.C), [3, 5]);
%! assert (s.C(:,3:4), zeros (3,2));
%! assert (s.df, 0);
%! assert (mu, [2, 1]);

%!test <*57964>
%! ## Disable warnings entirely because there is not a specific ID to disable.
%! wstate = warning ();
%! unwind_protect
%!   warning ("off", "all");
%!   [p, s] = polyfit ([1,2], [3,4], 2);
%! unwind_protect_cleanup
%!   warning (wstate);
%! end_unwind_protect
%! assert (p, [1/3, 0, 8/3], 6*eps);
%! assert (size (s.V), [2, 3]);
%! assert (s.V(:,2), [0; 0]);
%! assert (size (s.R), [2, 3]);
%! assert (s.R(:,2), [0; 0]);
%! assert (size (s.C), [2, 3]);
%! assert (s.C(:,2), [0; 0]);

## Verify Matlab-compatible return of upper powers
%!test <*67296>
%! wstate = warning ();
%! unwind_protect
%!   warning ("off", "all");
%!   x = 1:3;
%!   y = [0.9, 0.2, 0.3];
%!   p3 = polyfit (x, y, 3);
%!   p4 = polyfit (x, y, 4);
%! unwind_protect_cleanup
%!   warning (wstate);
%! end_unwind_protect
%! assert (p3, [0.1727, -0.6364, 0, 1.3636], 1e-4);
%! assert (p4, [0.0824, -0.2765, 0, 0, 1.0941], 1e-4);

## Verify Matlab-compatible corner case with only one output term
%!test <*67296>
%! wstate = warning ();
%! unwind_protect
%!   warning ("off", "all");
%!   p = polyfit (1,2,3);
%! unwind_protect_cleanup
%!   warning (wstate);
%! end_unwind_protect
%! assert (p, [2 0 0 0]);

## Test input validation
%!error <Invalid call> polyfit ()
%!error <Invalid call> polyfit (1)
%!error <Invalid call> polyfit (1,2)
%!error <X and Y must have the same number of points> polyfit ([1, 2], 1, 1)
%!error <X and Y must have the same number of points> polyfit (1, [1, 2], 1)
%!error <N must be a non-negative integer> polyfit (1, 2, [1,2])
%!error <N must be a non-negative integer> polyfit (1, 2, -1)
%!error <N must be a non-negative integer> polyfit (1, 2, Inf)
%!error <N must be a non-negative integer> polyfit (1, 2, 1.5)
%!test <*57964>
%! fail ("p = polyfit ([1,2], [3,4], 4)", "warning", "solution is not unique");
%!error <data points must be . 2 when scaling> [p,s,mu] = polyfit (1,2,3)
%!error <data points must be . 2 when scaling>
%! [p,s,mu] = polyfit ([1 2], [3 4], 3)
