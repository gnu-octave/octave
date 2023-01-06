########################################################################
##
## Copyright (C) 1998-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{q} =} quadl (@var{f}, @var{a}, @var{b})
## @deftypefnx {} {@var{q} =} quadl (@var{f}, @var{a}, @var{b}, @var{tol})
## @deftypefnx {} {@var{q} =} quadl (@var{f}, @var{a}, @var{b}, @var{tol}, @var{trace})
## @deftypefnx {} {@var{q} =} quadl (@var{f}, @var{a}, @var{b}, @var{tol}, @var{trace}, @var{p1}, @var{p2}, @dots{})
## @deftypefnx {} {[@var{q}, @var{nfev}] =} quadl (@dots{})
##
## Numerically evaluate the integral of @var{f} from @var{a} to @var{b} using
## an adaptive @nospell{Lobatto} rule.
##
## @var{f} is a function handle, inline function, or string containing the name
## of the function to evaluate.  The function @var{f} must be vectorized and
## return a vector of output values when given a vector of input values.
##
## @var{a} and @var{b} are the lower and upper limits of integration.  Both
## limits must be finite.
##
## The optional argument @var{tol} defines the absolute tolerance with which
## to perform the integration.  The default value is 1e-6.
##
## The algorithm used by @code{quadl} involves recursively subdividing the
## integration interval.  If @var{trace} is defined then for each subinterval
## display: (1) the total number of function evaluations, (2) the left end of
## the subinterval, (3) the length of the subinterval, (4) the approximation of
## the integral over the subinterval.
##
## Additional arguments @var{p1}, etc., are passed directly to the function
## @var{f}.  To use default values for @var{tol} and @var{trace}, one may pass
## empty matrices ([]).
##
## The result of the integration is returned in @var{q}.
##
## The optional output @var{nfev} indicates the total number of function
## evaluations performed.
##
## Reference: @nospell{W. Gander and W. Gautschi}, @cite{Adaptive Quadrature -
## Revisited}, BIT Vol.@: 40, No.@: 1, March 2000, pp.@: 84--101.
## @url{https://www.inf.ethz.ch/personal/gander/}
## @seealso{quad, quadv, quadgk, quadcc, trapz, dblquad, triplequad, integral,
##          integral2, integral3}
## @end deftypefn

## Original Author: Walter Gautschi
## Date: 08/03/98
## Reference: Gander, Computermathematik, Birkhaeuser, 1992.

## 2003-08-05 Shai Ayal
##   * permission from author to release as GPL

function [q, nfev] = quadl (f, a, b, tol = [], trace = false, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  if (isa (a, "single") || isa (b, "single"))
    eps = eps ("single");
  else
    eps = eps ("double");
  endif
  if (isempty (tol))
    tol = 1e-6;
  elseif (! isscalar (tol) || tol < 0)
    error ("quadl: TOL must be a scalar >=0");
  elseif (tol < eps)
    warning ("quadl: TOL specified is smaller than machine precision, using %g",
                                                                           tol);
    tol = eps;
  endif
  if (isempty (trace))
    trace = false;
  endif

  y = feval (f, [a, b], varargin{:});
  nfev = 1;

  fa = y(1);
  fb = y(2);

  h = b - a;

  [q, nfev, hmin] = adaptlobstp (f, a, b, fa, fb, Inf, nfev, abs (h),
                                 tol, trace, varargin{:});

  if (nfev > 10_000)
    warning ("quadl: maximum iteration count reached -- possible singular integral");
  elseif (any (! isfinite (q(:))))
    warning ("quadl: infinite or NaN function evaluations were returned");
  elseif (hmin < (b - a) * eps)
    warning ("quadl: minimum step size reached -- possible singular integral");
  endif

endfunction

function [q, nfev, hmin] = adaptlobstp (f, a, b, fa, fb, q0, nfev, hmin,
                                        tol, trace, varargin)

  persistent alpha = sqrt (2/3);
  persistent beta = 1 / sqrt (5);

  if (nfev > 10_000)
    q = q0;
    return;
  endif

  h = (b - a) / 2;
  m = (a + b) / 2;
  mll = m - alpha*h;
  ml  = m - beta*h;
  mr  = m + beta*h;
  mrr = m + alpha*h;
  x = [mll, ml, m, mr, mrr];
  y = feval (f, x, varargin{:});
  nfev += 1;
  fmll = y(1);
  fml  = y(2);
  fm   = y(3);
  fmr  = y(4);
  fmrr = y(5);
  i2 = (h/6)*(fa + fb + 5*(fml+fmr));
  i1 = (h/1470)*(77*(fa+fb) + 432*(fmll+fmrr) + 625*(fml+fmr) + 672*fm);

  if (abs (b - a) < hmin)
    hmin = abs (b - a);
  endif

  if (trace)
    disp ([nfev, a, b-a, i1]);
  endif

  ## Force at least one adaptive step (nfev > 2 test).
  if ((abs (i1-i2) < tol || mll <= a || b <= mrr) && nfev > 2)
    q = i1;
  else
    q = zeros (6, 1, class (x));
    [q(1), nfev, hmin] = adaptlobstp (f, a  , mll, fa  , fmll, q0/6, nfev, hmin,
                                      tol, trace, varargin{:});
    [q(2), nfev, hmin] = adaptlobstp (f, mll, ml , fmll, fml , q0/6, nfev, hmin,
                                      tol, trace, varargin{:});
    [q(3), nfev, hmin] = adaptlobstp (f, ml , m  , fml , fm  , q0/6, nfev, hmin,
                                      tol, trace, varargin{:});
    [q(4), nfev, hmin] = adaptlobstp (f, m  , mr , fm  , fmr , q0/6, nfev, hmin,
                                      tol, trace, varargin{:});
    [q(5), nfev, hmin] = adaptlobstp (f, mr , mrr, fmr , fmrr, q0/6, nfev, hmin,
                                      tol, trace, varargin{:});
    [q(6), nfev, hmin] = adaptlobstp (f, mrr, b  , fmrr, fb  , q0/6, nfev, hmin,
                                      tol, trace, varargin{:});
    q = sum (q);
  endif

endfunction


## basic functionality
%!assert (quadl (@(x) sin (x), 0, pi), 2, 1e-6)

## the values here are very high so it may be unavoidable that this fails
%!assert (quadl (@(x) sin (3*x).*cosh (x).*sinh (x),10,15, 1e-3),
%!        2.588424538641647e+10, 1e-3)

## extra parameters
%!assert (quadl (@(x,a,b) sin (a + b*x), 0, 1, [], [], 2, 3),
%!        cos (2)/3 - cos (5)/3, 1e-6)

## test different tolerances.
%!test
%! [q, nfev1] = quadl (@(x) sin (2 + 3*x).^2, 0, 10, 0.5, []);
%! assert (q, (60 + sin (4) - sin (64))/12, 0.5);
%! [q, nfev2] = quadl (@(x) sin (2 + 3*x).^2, 0, 10, 0.1, []);
%! assert (q, (60 + sin (4) - sin (64))/12, 0.1);
%! assert (nfev2 > nfev1);

%!test  # test single input/output
%! assert (class (quadl (@sin, 0, 1)), "double");
%! assert (class (quadl (@sin, single (0), 1)), "single");
%! assert (class (quadl (@sin, 0, single (1))), "single");

## Test input validation
%!error <Invalid call> quadl ()
%!error <Invalid call> quadl (@sin)
%!error <Invalid call> quadl (@sin,1)
%!error <TOL must be a scalar> quadl (@sin,0,1, ones (2,2))
%!error <TOL must be .* .=0> quadl (@sin,0,1, -1)
