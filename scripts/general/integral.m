########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{q} =} integral (@var{f}, @var{a}, @var{b})
## @deftypefnx {} {@var{q} =} integral (@var{f}, @var{a}, @var{b}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {[@var{q}, @var{err}] =} integral (@dots{})
##
## Numerically evaluate the integral of @var{f} from @var{a} to @var{b} using
## adaptive quadrature.
##
## @code{integral} is a wrapper for @code{quadcc} (general real-valued, scalar
## integrands and limits), and @code{quadgk} (integrals with specified
## integration paths and array-valued integrands) that is intended to provide
## @sc{matlab} compatibility.  More control of the numerical integration may be
## achievable by calling the various quadrature functions directly.
##
## @var{f} is a function handle, inline function, or string containing the name
## of the function to evaluate.  The function @var{f} must be vectorized and
## return a vector of output values when given a vector of input values.
##
## @var{a} and @var{b} are the lower and upper limits of integration.  Either
## or both limits may be infinite or contain weak end singularities.  If either
## or both limits are complex, @code{integral} will perform a straight line
## path integral.  Alternatively, a complex domain path can be specified using
## the @qcode{"Waypoints"} option (see below).
##
## Additional optional parameters can be specified using
## @qcode{"@var{property}", @var{value}} pairs.  Valid properties are:
##
## @table @code
## @item Waypoints
## Specifies points to be used in defining subintervals of the quadrature
## algorithm, or if @var{a}, @var{b}, or @var{waypoints} are complex then
## the quadrature is calculated as a contour integral along a piecewise
## continuous path.  For more detail, @pxref{XREFquadgk,,@code{quadgk}}.
##
## @item ArrayValued
## @code{integral} expects @var{f} to return a scalar value unless
## @var{arrayvalued} is specified as true.  This option will cause
## @code{integral} to perform the integration over the entire array and return
## @var{q} with the same dimensions as returned by @var{f}.  For more detail
## @pxref{XREFquadgk,,@code{quadgk}}.
##
## @item AbsTol
## Define the absolute error tolerance for the quadrature.  The default
## absolute tolerance is 1e-10 (1e-5 for single).
##
## @item RelTol
## Define the relative error tolerance for the quadrature.  The default
## relative tolerance is 1e-6 (1e-4 for single).
## @end table
##
## The optional output @var{err} contains the absolute error estimate used by
## the called integrator.
##
## Adaptive quadrature is used to minimize the estimate of error until the
## following is satisfied:
## @tex
## $$error \leq \max \left( AbsTol, RelTol\cdot\vert q\vert \right)$$
## @end tex
## @ifnottex
##
## @example
## @group
##   @var{error} <= max (@var{AbsTol}, @var{RelTol}*|@var{q}|).
## @end group
## @end example
##
## @end ifnottex
##
## Known @sc{matlab} incompatibilities:
##
## @enumerate
## @item
## If tolerances are left unspecified, and any integration limits or waypoints
## are of type @code{single}, then Octave's integral functions automatically
## reduce the default absolute and relative error tolerances as specified
## above.  If tighter tolerances are desired they must be specified.
## @sc{matlab} leaves the tighter tolerances appropriate for @code{double}
## inputs in place regardless of the class of the integration limits.
## @end enumerate
##
## @seealso{integral2, integral3, quad, quadgk, quadv, quadl, quadcc, trapz,
##          dblquad, triplequad}
## @end deftypefn

function [q, err] = integral (f, a, b, varargin)

  if (nargin < 3 || (mod (nargin, 2) == 0))
    print_usage ();
  endif

  error_flag = (nargout == 2);

  ## quadcc can't handle complex limits or integrands, but quadgk can.
  ## Check for simple cases of complex limits and integrand.
  f_is_complex = false;
  if (iscomplex (a) || iscomplex (b))
    f_is_complex = true;
  elseif (iscomplex (feval (f, a)) || iscomplex (feval (f, b)))
    f_is_complex = true;
  endif

  if (nargin == 3)
    ## Pass the simplest case directly to general integrator.
    ## Let quadcc function handle input checks on function and limits.
    if (! f_is_complex)
      try
        if (error_flag)
          [q, err] = quadcc (f, a, b);
        else
          q = quadcc (f, a, b);
        endif
      catch quaderror
        if (strcmp (quaderror.message,
                "quadcc: integrand F must return a single, real-valued vector"))
          if (error_flag)
            [q, err] = quadgk (f, a, b);
          else
            q = quadgk (f, a, b);
          endif
        else
          error (quaderror.message);
        endif
      end_try_catch

    else
      ## Complex-valued integral
      if (error_flag)
        [q, err] = quadgk (f, a, b);
      else
        q = quadgk (f, a, b);
      endif
    endif

  else
    ## Parse options to determine how to call integrator.
    abstol = [];
    reltol = [];
    waypoints = [];
    arrayvalued = false;
    use_quadgk = false;

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
          use_quadgk = true;
        case "arrayvalued"
          arrayvalued = varargin{idx++};
          use_quadgk = true;
        otherwise
          error ("integral: unknown property '%s'", prop);
      endswitch
    endwhile

    issingle = (isa (a, "single") || isa (b, "single")
                || isa (waypoints, "single"));

    if (isempty (abstol))
      abstol = ifelse (issingle, 1e-5, 1e-10);
    endif
    if (isempty (reltol))
      reltol = ifelse (issingle, 1e-4, 1e-6);
    endif

    if (use_quadgk)
      ## Array valued functions or waypoint definitions require quadgk
      ## no need to test for complex components
      if (error_flag)
        [q, err] = quadgk (f, a, b, "AbsTol", abstol, "RelTol", reltol,
                            "WayPoints", waypoints, "ArrayValued", arrayvalued);
      else
        q = quadgk (f, a, b, "AbsTol", abstol, "RelTol", reltol,
                            "WayPoints", waypoints, "ArrayValued", arrayvalued);
      endif

    else
      ## otherwise try quadcc first, switch to quadgk if complex test fails
      if (! f_is_complex)
        try
          if (error_flag)
            [q, err] = quadcc (f, a, b, [abstol, reltol]);
          else
            q = quadcc (f, a, b, [abstol, reltol]);
          endif
        catch quaderror
          if (strcmp (quaderror.message,
                "quadcc: integrand F must return a single, real-valued vector"))
            if (error_flag)
              [q, err] = quadgk (f, a, b, "AbsTol", abstol, "RelTol", reltol);
            else
              q = quadgk (f, a, b, "AbsTol", abstol, "RelTol", reltol);
            endif
          else
            error (quaderror.message);
          endif
        end_try_catch
      else
        ## Complex-valued integral
        if (error_flag)
          [q, err] = quadgk (f, a, b, "AbsTol", abstol, "RelTol", reltol);
        else
          q = quadgk (f, a, b, "AbsTol", abstol, "RelTol", reltol);
        endif
      endif
    endif
  endif

endfunction


## Matlab compatibility tests
%!test
%! f = @(x) exp (-x.^2) .* log (x).^2;
%! emgamma = 0.57721566490153286;
%! exact = (sqrt (pi)*(8*log (2)^2+8*emgamma*log (2)+pi^2+2*emgamma^2))/16;
%! assert (integral (f, 0, Inf), exact, -1e-6);
%! assert (integral (f, 0, Inf, "RelTol", 1e-12), exact, -1e-12);

%!test  # with parameter
%! f = @(x, c) 1 ./ (x.^3 - 2*x - c);
%! assert (integral (@(x) f(x,5), 0, 2), -0.4605015338467329, 1e-10);

%!test  # with tolerances
%! f = @(x) log (x);
%! assert (integral (@(x) f(x), 0, 1, "AbsTol", 1e-6), -1, 1e-6);

%!test  # waypoints
%! f = @(x) 1./(2.*x-1);
%! assert (integral (f, 0, 0, "Waypoints", [1+1i, 1-1i]), -pi*1i, 1e-10);

%!test  # an array-valued function
%! f = @(x) sin ((1:5)*x);
%! assert (integral (f, 0, 1, "ArrayValued", true), 1./[1:5]-cos(1:5)./[1:5],
%!         1e-10);

%!test  # test single input/output
%! assert (integral (@sin, 0, 1), cos (0)-cos (1), 1e-10);
%! assert (class (integral (@sin, single (0), 1)), "single");
%! assert (class (integral (@sin, 0, single (1))), "single");
%! assert (class (integral (@sin, single (0), single (1))), "single");
%! assert (integral (@sin, 0, 1, "Waypoints", 0.5), cos (0)-cos (1), 1e-10);
%! assert (class (integral (@sin, 0, 1, "Waypoints", single (0.5))), "single");
%! assert (class (integral (@sin, single (0), 1, "Waypoints", 0.5)), "single");
%! assert (class (integral (@sin, 0, single (1), "Waypoints", 0.5)), "single");

%!test  # test complex argument handling
%! f = @(x) round (exp (i*x));
%! assert (integral (f, 0, pi), quadgk (f, 0, pi), eps);
%! assert (integral (f, -1, 1), 2, 5*eps);
%! assert (integral (@sin, -i, i), 0, eps);
%! assert (1.5 * integral (@sqrt, -1, 0), i, eps);

%!test
%! f = @(x) x.^5 .* exp (-x) .* sin (x);
%! assert (integral (f, 0, inf, "RelTol", 1e-8, "AbsTol", 1e-12), -15, -1e-8);

## tests from quadcc
%!assert (integral (@sin, -pi, pi), 0, 1e-10)
%!assert (integral (inline ("sin"), -pi, pi), 0, 1e-10)
%!assert (integral ("sin", -pi, pi), 0, 1e-10)
%!assert (integral (@sin, -pi, 0), -2, 1e-10)
%!assert (integral (@sin, 0, pi), 2, 1e-10)
%!assert (integral (@(x) 1./(sqrt (x).*(x+1)), 0, Inf), pi, -1e-6)
%!assert (integral (@(x) 1./(sqrt (x).*(x+1)), 0, Inf,
%!                  "AbsTol", 0, "RelTol", 1e-8),
%!        pi, -1e-8)
%!assert (integral (@(x) exp (-x .^ 2), -Inf, Inf), sqrt (pi), 1e-10)
%!assert (integral (@(x) exp (-x .^ 2), -Inf, 0), sqrt (pi)/2, 1e-10)

## tests from quadgk
%!assert (integral (@sin,-pi,pi, "WayPoints",0, "AbsTol",1e-6, "RelTol",1e-3),
%!        0, 1e-6)
%!assert (integral (@(x) abs (1 - x.^2), 0, 2, "Waypoints", 1), 2, 1e-10)
%!assert (integral (@(z) log (z),1+1i,1+1i, "WayPoints", [1-1i, -1,-1i, -1+1i]),
%!        complex (0, pi), 1e-10)

## Test vector-valued functions
%!assert (integral (@(x) [(sin (x)), (sin (2*x))], 0, pi, "ArrayValued", 1),
%!        [2, 0], 1e-10)

## Test matrix-valued functions
%!assert (integral (@(x) [x,x,x; x,exp(x),x; x,x,x], 0, 1, "ArrayValued", 1),
%!                    [0.5,0.5,0.5; 0.5,(exp (1) - 1),0.5; 0.5,0.5,0.5], 1e-10);

## Test combined parameters
%!assert (integral (@(x) [sin(x), cos(x)], 0, pi, "ArrayValued", 1,
%!                   "Waypoints", [0.5]), [2, 0], 2*eps);

## Test 2nd output
%!test <*62412>
%! [~, err] = integral (@(x) ones (size (x)), 0, 1);  # quadcc
%! assert (err, 0, 5*eps);  # err ~3e-16
%! [~, err] = integral (@(x) ones (size (x)), 0, 1, "waypoints", 1);  # quadgk
%! assert (err, 0, 1000*eps);  # err ~7e-14
%! [~, err] = integral (@(x) ones (size (x)), 0, 1, "arrayvalued", true);  # quadgk
%! assert (err, 0, 1000*eps);  # err ~7e-14

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
