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
## @deftypefn  {} {@var{q} =} integral2 (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb})
## @deftypefnx {} {@var{q} =} integral2 (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{prop}, @var{val}, @dots{})
##
## Numerically evaluate the two-dimensional integral of @var{f} using adaptive
## quadrature over the two-dimensional domain defined by @var{xa}, @var{xb},
## @var{ya}, @var{yb} (scalars may be finite or infinite).
##
## @code{integral2} is a wrapper for @code{dblquad} intended to provide
## @sc{matlab} compatibility.  More control of the numerical integration may
## be achievable by calling the various quadrature functions directly.
##
## @var{f} is a function handle, inline function, or string containing the name
## of the function to evaluate.  The function @var{f} must be of the form
## @math{z = f(x,y)} where @var{x} is a vector and @var{y} is a scalar.  It
## should return a vector of the same length and orientation as @var{x}.
##
## Additional optional parameters can be specified using
## @qcode{"@var{property}", @var{value}} pairs.  Valid properties are:
##
## @table @code
## @item AbsTol
## Define the absolute error tolerance for the quadrature.  The default
## value is 1e-10 (1e-5 for single).
##
## @item RelTol
## Define the relative error tolerance for the quadrature.  The default
## value is 1e-6 (1e-4 for single).
## @end table
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
#
## @item
## @code{integral2} currently only integrates functions over rectangular
## domains.  Implementing @var{ya} and @var{yb} as functions of @var{x} is a
## planned future improvement.
##
## @item
## The @qcode{"Method"} property is not yet implemented in Octave due to the
## lack of a @qcode{"tiled"} integrator implementation.  All integrals are
## evaluated using an equivalent of the @qcode{"iterated"} method.
## @end enumerate
##
## @seealso{integral, integral3, quad, quadgk, quadv, quadl, quadcc, trapz,
##          dblquad, triplequad}
## @end deftypefn

function q = integral2 (f, xa, xb, ya, yb, varargin)
  ## FIXME: It is possible that a non-rectangular domain could be handled by
  ##        overlaying the integrand with a boolean mask function such that
  ##        the integration occurs over a rectangle, but regions outside the
  ##        desired domain contribute zero to the integral.  This may be an
  ##        inefficient but acceptable hack to get around the rectangular
  ##        domain limit without having to rewrite the integrating function.

  ## FIXME: Implement "Method" property to let the user select between iterated
  ##        and tiled integration.  Tiled integration follows the method of
  ##        Matlab's quad2d function, currently unimplemented in Octave.
  ##        Should probably just wait for a quad2d implementation to point the
  ##        integral2 wrapper to, instead of trying to re-create it here.  The
  ##        following can be added to the help docstring once it is functional:
  ## @item Method
  ## Specifies the two dimensional integration method to be used, with valid
  ## options being @var{"auto"}, @var{"tiled"}, or @var{"iterated"}.
  ## @code{integral} will use @var{"auto"} by default, where it will usually
  ## choose @var{"tiled"} unless any of the integration limits are infinite.

  if (nargin < 5 || (mod (nargin, 2) == 0))
    print_usage ();
  endif

  if (! is_function_handle (f))
    print_usage ();
  endif

  if (! (isscalar (xa) && isscalar (xb) && isscalar (ya)) && isscalar (yb))
    print_usage ();
  endif

  ## Check for single or double limits to set appropriate default tolerance.
  issingle = isa ([xa, xb, ya, yb], "single");

  ## Set defaults, update with any specified parameters.
  if (issingle)
    abstol = 1e-5;
    reltol = 1e-4;
  else
    abstol = 1e-10;
    reltol = 1e-6;
  endif

  if (nargin == 5)
    ## Pass the simplest case directly to integrator.
    ## Let quadcc function handle input checks.
    q = dblquad (f, xa, xb, ya, yb, [abstol, reltol], @quadcc);

  else
    ## Parse options to determine how to call integrator.
    intmethod = [];

    idx = 1;
    while (idx < nargin - 5)
      prop = varargin{idx++};
      if (! ischar (prop))
        error ("integral2: property PROP must be a string");
      endif

      switch (tolower (prop))
        case "abstol"
          abstol = varargin{idx++};
          if (! (isnumeric (abstol) && isscalar (abstol) && abstol >= 0))
            error ("integral2: AbsTol value must be a numeric scalar >= 0");
          endif

        case "reltol"
          reltol = varargin{idx++};
          if (! (isnumeric (reltol) && isscalar (reltol) && reltol >= 0))
            error ("integral2: RelTol value must be a numeric scalar >= 0");
          endif

        case "method"
          intmethod = varargin{idx++};
          warning (["integral2: Only 'iterated' method implemented.  ", ...
                    "Method property ignored."]);
        otherwise
          error ("integral2: unknown property '%s'", prop);
      endswitch
    endwhile

    q = dblquad (f, xa, xb, ya, yb, [abstol, reltol], @quadcc);

  endif

endfunction


%!test
%! f = @(x, y) x .* y;
%! assert (integral2 (f, 0, 1, 0, 1), 0.25, 1e-10);

%!test
%! f = @(x, y) 9 * x.^2 + 15 * y.^2;
%! assert (integral2 (f, 0, 5, -5, 0, "AbsTol", 1e-9), 5000, 1e-9);
%! assert (integral2 (f, 0, 5, -5, 0, "RelTol", 1e-6), 5000, -1e-6);
%! assert (integral2 (f, 0, 5, -5, 0, "RelTol", 1e-6, "AbsTol", 1e-9),
%!         5000, 1e-9);

## tests from dblquad
%!assert (integral2 (@(x, y) 1 ./ (x+y), 0, 1, 0, 1, "AbsTol", 1e-7),
%!        2*log (2), 1e-7)
%!assert (integral2 (@(x, y) 1 ./ (x+y), 0, 1, 0, 1, "RelTol", 1e-6),
%!        2*log (2), -1e-6)
%!assert (integral2 (@(x, y) 1 ./ (x+y), 0, 1, 0, 1, "AbsTol", 1e-8,
%!        "RelTol", 1e-6), 2*log (2), -1e-6)
%!assert (integral2 (@(x, y) exp (-x.^2 - y.^2) , -1, 1, -1, 1),
%!        pi * erf (1).^2, 1e-10)

%!assert (integral2 (@plus, 1, 2, 3, 4), 5, 1e-10)

## Test input validation
%!error integral2
%!error integral2 (0, 1 ,2 ,3 ,4)
%!error integral2 (@plus)
%!error integral2 (@plus, 1)
%!error integral2 (@plus, 1, 2)
%!error integral2 (@plus, 1, 2, 3)
%!error integral2 (@plus, 1, 2, 3, [4 5])
%!error integral2 (@plus, 1, 2, 3, "test")
%!error integral2 (@plus, 1, 2, 3, 4, "foo")
%!error <unknown property 'foo'>  integral2 (@plus, 1, 2, 3, 4, "foo", "bar")
%!error <property PROP must be a string> integral2 (@plus, 1, 2, 3, 4, 99, "bar")
%!error <AbsTol value must be a numeric> integral2 (@plus, 1, 2, 3, 4, "AbsTol", "foo")
%!error <AbsTol value must be a .* scalar> integral2 (@plus, 1, 2, 3, 4, "AbsTol", [1, 2])
%!error <AbsTol value must be.* .= 0> integral2 (@plus, 1, 2, 3, 4, "AbsTol", -1)
%!error <RelTol value must be a numeric> integral2 (@plus, 1, 2, 3, 4, "RelTol", "foo")
%!error <RelTol value must be a .* scalar> integral2 (@plus, 1, 2, 3, 4, "RelTol", [1, 2])
%!error <RelTol value must be.* .= 0> integral2 (@plus, 1, 2, 3, 4, "RelTol", -1)
%!warning <Only 'iterated' method implemented>
%! q = integral2 (@plus, 0, 1, 0, 1, "Method", "tiled");
