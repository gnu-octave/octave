########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{q} =} quadv (@var{f}, @var{a}, @var{b})
## @deftypefnx {} {@var{q} =} quadv (@var{f}, @var{a}, @var{b}, @var{tol})
## @deftypefnx {} {@var{q} =} quadv (@var{f}, @var{a}, @var{b}, @var{tol}, @var{trace})
## @deftypefnx {} {@var{q} =} quadv (@var{f}, @var{a}, @var{b}, @var{tol}, @var{trace}, @var{p1}, @var{p2}, @dots{})
## @deftypefnx {} {[@var{q}, @var{nfev}] =} quadv (@dots{})
##
## Numerically evaluate the integral of @var{f} from @var{a} to @var{b}
## using an adaptive Simpson's rule.
##
## @var{f} is a function handle, inline function, or string containing the name
## of the function to evaluate.  @code{quadv} is a vectorized version of
## @code{quad} and the function defined by @var{f} must accept a scalar or
## vector as input and return a scalar, vector, or array as output.
##
## @var{a} and @var{b} are the lower and upper limits of integration.  Both
## limits must be finite.
##
## The optional argument @var{tol} defines the absolute tolerance used to stop
## the adaptation procedure.  The default value is 1e-6.
##
## The algorithm used by @code{quadv} involves recursively subdividing the
## integration interval and applying Simpson's rule on each subinterval.
## If @var{trace} is true then after computing each of these partial
## integrals display: (1) the total number of function evaluations,
## (2) the left end of the subinterval, (3) the length of the subinterval,
## (4) the approximation of the integral over the subinterval.
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
## Note: @code{quadv} is written in Octave's scripting language and can be
## used recursively in @code{dblquad} and @code{triplequad}, unlike the
## @code{quad} function.
## @seealso{quad, quadl, quadgk, quadcc, trapz, dblquad, triplequad, integral,
##          integral2, integral3}
## @end deftypefn

## Algorithm: See https://en.wikipedia.org/wiki/Adaptive_Simpson%27s_method
## for basic explanation.  See NOTEs and FIXME for Octave modifications.

function [q, nfev] = quadv (f, a, b, tol = [], trace = false, varargin)

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
    error ("quadv: TOL must be a scalar >=0");
  endif

  if (trace)
    ## Print column headers once above trace display.
    printf ("  nfev          a            (b - a)         q_interval\n");
  endif

  ## NOTE: Split the interval into 3 parts which avoids problems with periodic
  ## functions when a, b, and (a + b)/2 fall on boundaries such as 0, 2*pi.
  ## For compatibility with Matlab, split in to two equal size regions on the
  ## left and right, and one larger central region.
  alpha = 0.27158;   # factor for region 1 & region 3 size (~27%)
  b1 = a + alpha * (b - a);
  b2 = b - alpha * (b - a);
  c1 = (a + b1) / 2;
  c2 = (a + b)  / 2;
  c3 = (b2 + b) / 2;

  fa  = feval (f, a,  varargin{:});
  fc1 = feval (f, c1, varargin{:});
  fb1 = feval (f, b1, varargin{:});
  fc2 = feval (f, c2, varargin{:});
  fb2 = feval (f, b2, varargin{:});
  fc3 = feval (f, c3, varargin{:});
  fb  = feval (f, b,  varargin{:});
  nfev = 7;

  ## NOTE: If there are edge singularities, move edge point by eps*(b-a) as
  ## discussed in Shampine paper used to implement quadgk.
  if (any (isinf (fa(:))))
    fa = feval (f, a + eps * (b-a), varargin{:});
    nfev++;
  endif
  if (any (isinf (fb(:))))
    fb = feval (f, b - eps * (b-a), varargin{:});
    nfev++;
  endif

  ## Region 1
  h = (b1 - a);
  q1 = h / 6 * (fa + 4*fc1 + fb1);

  [q1, nfev, hmin1] = simpsonstp (f, a, b1, c1, fa, fb1, fc1, q1, tol,
                                  nfev, abs (h), trace, varargin{:});

  ## Region 2
  h = (b2 - b1);
  q2 = h / 6 * (fb1 + 4*fc2 + fb2);

  [q2, nfev, hmin2] = simpsonstp (f, b1, b2, c2, fb1, fb2, fc2, q2, tol,
                                  nfev, abs (h), trace, varargin{:});

  ## Region 3
  h = (b - b2);
  q3 = h / 6 * (fb2 + 4*fc3 + fb);

  [q3, nfev, hmin3] = simpsonstp (f, b2, b, c3, fb2, fb, fc3, q3, tol,
                                  nfev, abs (h), trace, varargin{:});

  ## Total integral over all 3 regions and verify results
  q = q1 + q2 + q3;

  hmin = min ([hmin1, hmin2, hmin3]);

  if (nfev > 10_000)
    warning ("quadv: maximum iteration count reached -- possible singular integral");
  elseif (any (! isfinite (q(:))))
    warning ("quadv: infinite or NaN function evaluations were returned");
  elseif (hmin < (b - a) * eps)
    warning ("quadv: minimum step size reached -- possible singular integral");
  endif

endfunction

function [q, nfev, hmin] = simpsonstp (f, a, b, c, fa, fb, fc, q0, tol,
                                       nfev, hmin, trace, varargin)

  if (nfev > 10_000)   # stop endless recursion
    q = q0;
    return;
  endif

  d = (a + c) / 2;
  e = (c + b) / 2;
  fd = feval (f, d, varargin{:});
  fe = feval (f, e, varargin{:});
  nfev += 2;
  q1 = (c - a) / 6 * (fa + 4*fd + fc);
  q2 = (b - c) / 6 * (fc + 4*fe + fb);
  q = q1 + q2;

  if (abs (a - c) < hmin)
    hmin = abs (a - c);
  endif

  delta = q - q0;   # error term between new estimate and old estimate

  if (trace)
    printf ("%5d   %#14.11g   %16.10e   %-16.11g\n",
            nfev,  a,         b-a,      q + delta/15);
  endif

  ## NOTE: Not vectorizing q-q0 in the norm provides a more rigid criterion
  ##       for matrix-valued functions.
  if (norm (delta, Inf) > 15*tol)
    ## FIXME: To keep sum of sub-interval integrands within overall tolerance
    ## each bisection interval should use tol/2.  However, Matlab does not
    ## do this, and it would also profoundly increase the number of function
    ## evaluations required.
    [q1, nfev, hmin] = simpsonstp (f, a, c, d, fa, fc, fd, q1, tol,
                                   nfev, hmin, trace, varargin{:});
    [q2, nfev, hmin] = simpsonstp (f, c, b, e, fc, fb, fe, q2, tol,
                                   nfev, hmin, trace, varargin{:});
    q = q1 + q2;
  else
    q += delta / 15;   # NOTE: Richardson extrapolation correction
  endif

endfunction


%!assert (quadv (@sin, 0, 2*pi), 0, 1e-6)
%!assert (quadv (@sin, 0, pi), 2, 1e-6)

## Test weak singularities at the edge
%!assert (quadv (@(x) 1 ./ sqrt (x), 0, 1), 2, 15*1e-6)

## Test vector-valued functions
%!assert (quadv (@(x) [(sin (x)), (sin (2 * x))], 0, pi), [2, 0], 1e-6)

## Test matrix-valued functions
%!assert (quadv (@(x) [ x,x,x; x,1./sqrt(x),x; x,x,x ], 0, 1),
%!        [0.5,0.5,0.5; 0.5,2,0.5; 0.5,0.5,0.5], 15*1e-6);

## Test periodic function
%!assert <*57603> (quadv (@(t) sin (t) .^ 2, 0, 8*pi), 4*pi, 1e-6)

## Test input validation
%!error <Invalid call> quadv ()
%!error <Invalid call> quadv (@sin)
%!error <Invalid call> quadv (@sin,1)
%!error <TOL must be a scalar> quadv (@sin,0,1, ones (2,2))
%!error <TOL must be .* .=0> quadv (@sin,0,1, -1)
