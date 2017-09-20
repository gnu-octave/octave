## Copyright (C) 2017 Nicholas Jankowski
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {@var{q} =} integral (@var{f}, @var{a}, @var{b})
## @deftypefnx {} {@var{q} =} integral (@var{f}, @var{a}, @var{b}, @var{prop}, @var{val}, @dots{})
##
## Numerically evaluate the integral of @var{f} from @var{a} to @var{b} using
## adaptive quadrature.
##
## @code{integral} is a wrapper for @code{quadgk} (for scalar integrands) and
## @code{quadv} (for array-valued integrands) intended to provide Matlab
## compatibility. More control of the numerical integration may be achievable
## by calling the various quadrature functions directly.
##
## @var{f} is a function handle, inline function, or string containing the name
## of the function to evaluate.  The function @var{f} must be vectorized and
## return a vector of output values when given a vector of input values.
##
## @var{a} and @var{b} are the lower and upper limits of integration.  Either
## or both limits may be infinite or contain weak end singularities.  If either
## or both limits are complex, @code{integral} will perform a straight line
## path integral.  Alternatively, a complex domain path can be specified using
## the "Waypoints" option (see below).
##
## Additional optional parameters can be specified using
## @qcode{"@var{property}", @var{value}} pairs.  Valid properties are:
##
## @table @code
## @item AbsTol
## Define the absolute error tolerance for the quadrature.  The default
## absolute tolerance is 1e-10 (1e-5 for single).
##
## @item RelTol
## Define the relative error tolerance for the quadrature.  The default
## relative tolerance is 1e-6 (1e-4 for single).
##
## @item Waypoints
## Specifies points to be used in defining subintervals of the quadrature
## algorithm, or if @var{a}, @var{b}, or @var{waypoints} are complex then
## the quadrature is calculated as a contour integral along a piecewise
## continuous path.  For more detail see @code{quadgk}.
##
## @item ArrayValued
## @code{integral} expects @var{f} to return a scalar value unless
## @var{arrayvalued} is specified as true.  This option will cause
## @code{integral} to perform the integration over the entire array and return
## @var{q} with the same dimensions as returned by @var{f}.
## @end table
##
## Implementation Note: As a consequence of using @code{quadgk} and
## @code{quadv}, certain option combinations are currently unsupported.
## @qcode{"ArrayValued"} cannot be combined with @qcode{"RelTol"} or
## @qcode{"Waypoints"}.  This is a known incompatibility with Matlab.
##
## @seealso{quad, quadgk, quadv, quadl, quadcc, trapz, dblquad, triplequad}
## @end deftypefn

function q = integral (f, a, b, varargin)

  if (nargin < 3 || (mod (nargin, 2) == 0))
    print_usage ();
  endif

  if (nargin == 3)
    ## Pass the simplest case directly to general integrator.
    ## Let quadgk function handle input checks on function and limits.
    q = quadgk (f, a, b);
  else
    ## Parse options to determine how to call integrator
    abstol = [];
    reltol = [];
    waypoints = [];
    arrayvalued = false;

    idx = 1;
    while (idx < nargin - 3)
      prop = varargin{idx++};
      if (! ischar (prop))
        error ("integral: property PROP must be a string");
      endif

      switch (tolower (prop))
        case "reltol"
          reltol = varargin{idx++};
        case "abstol"
          abstol = varargin{idx++};
        case "waypoints"
          waypoints = varargin{idx++}(:);
        case "arrayvalued"
          arrayvalued = varargin{idx++};
        otherwise
          error ("integral: unknown property '%s'", prop);
      endswitch
    endwhile

    if (arrayvalued)
      ## FIXME: replace warning with arrayfun(?) call to quadgk
      if (! isempty (waypoints))
        warning(["integral: array-valued quadrature routine currently ", ...
                 "unable to handle WayPoints.  WayPoints are ignored."]);
      endif

      ## FIXME: remove warning once we have reltol compatible arrayval'd
      ## quad or arrayfun call to quadgk.
      if (! isempty (reltol))
        warning(["integral: array-valued quadrature only accepts AbsTol.", ...
                 "  RelTol ignored."]);
      endif

      ## quadv accepts empty abstol input.
      q = quadv (f, a, b, abstol);

    else
      ## quadgk will accept empty values of optional parameters
      q = quadgk (f, a, b, "AbsTol", abstol, "RelTol", reltol, ...
                  "WayPoints", waypoints);

    endif
  endif

endfunction


## Matlab compatibility tests
%!test
%! f = @(x) exp (-x.^2) .* log (x).^2;
%! emgamma = 0.57721566490153286;
%! exact = (sqrt (pi)*(8*log (2)^2+8*emgamma*log (2)+pi^2+2*emgamma^2))/16;
%! assert (integral (f, 0, Inf), exact, 1e-6);
%! assert (integral (f, 0, Inf, "RelTol", 1e-12), exact, 1e-12);

%!test  # with parameter
%! f = @(x, c) 1 ./ (x.^3 - 2*x - c);
%! assert (integral (@(x) f(x,5), 0, 2), -0.4605015338467329, 1e-12);

%!test  # with tolerances
%! f = @(x) log(x);
%! assert (integral (@(x) f(x), 0, 1, "AbsTol", 1e-6), -1, 1e-6);

%!test  # waypoints
%! f = @(x) 1./(2.*x-1);
%! assert (integral (f, 0, 0, "Waypoints", [1+1i, 1-1i]), -pi*1i, 1e-12);

%!test  # test array function
%! f = @(x) sin ((1:5)*x);
%! assert (integral (f, 0, 1, "ArrayValued", true), 1./[1:5]-cos(1:5)./[1:5],
%!         1e-10);

%!test
%! f = @(x) x.^5 .* exp (-x) .* sin (x);
%! assert (integral (f, 0, inf, "RelTol", 1e-8, "AbsTol", 1e-12), -15, 2e-12);

## tests from quadgk
%!test
%!assert (integral (@sin,-pi,pi), 0, 1e-6);
%!assert (integral (inline ("sin"),-pi,pi), 0, 1e-6);
%!assert (integral ("sin",-pi,pi), 0, 1e-6);
%!assert (integral (@sin,-pi,pi), 0, 1e-6);

%!assert (integral (@sin,-pi,0), -2, 1e-6);
%!assert (integral (@sin,0,pi), 2, 1e-6);
%!assert (integral (@(x) 1./sqrt (x),0,1), 2, 1e-6);
%!assert (integral (@(x) abs (1 - x.^2),0,2, "Waypoints", 1), 2, 1e-6);
%!assert (integral (@(x) 1./(sqrt (x) .* (x+1)),0,Inf), pi, 1e-6);
%!assert (integral (@(z) log (z),1+i,1+i, "WayPoints", [1-i, -1,-i, -1+i]), ...
%!        -pi * 1i, 1e-6);
%!assert (integral (@(x) exp (-x .^ 2),-Inf,Inf), sqrt (pi), 1e-6);
%!assert (integral (@(x) exp (-x .^ 2),-Inf,0), sqrt (pi)/2, 1e-6);

## tests from quadv
## Test vector-valued functions
%!assert (integral (@(x) [(sin (x)), (sin (2*x))], 0, pi, "ArrayValued", 1), ...
%!        [2, 0], 1e-5);

## Test matrix-valued functions
%!test
%! warning ("off", "Octave:divide-by-zero", "local");
%! assert (integral (@(x) [x, x, x; x, 1./sqrt(x), x; x, x, x], 0, 1, ...
%!                   "ArrayValued", 1), ...
%!         [0.5, 0.5, 0.5; 0.5, 2, 0.5; 0.5, 0.5, 0.5], 1e-5);

## Test input validation
%!error integral (@sin)
%!error integral (@sin, 0)
%!error integral (@sin, 0, 1, 1e-6, true, 4)
%!error integral (@sin, 0, 1, "DummyArg")
%!error <property PROP must be a string> integral (@sin, 0, 1, 2, 3)
%!error <unknown property 'foo'> integral (@sin, 0, 1, "foo", 3)
%!error integral (@sin, 0, 1, "AbsTol", ones (2,2))
%!error integral (@sin, 0, 1, "AbsTol", -1)
%!error integral (@sin, 0, 1, "RelTol", ones (2,2))
%!error integral (@sin, 0, 1, "RelTol", -1)
