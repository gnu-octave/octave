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
## @deftypefn  {} {@var{q} =} integral3 (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{za}, @var{zb})
## @deftypefnx {} {@var{q} =} integral3 (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{za}, @var{zb}, @var{prop}, @var{val}, @dots{})
##
## Numerically evaluate the three-dimensional integral of @var{f} using
## adaptive quadrature over the three-dimensional domain defined by
## @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{za}, @var{zb} (scalars may
## be finite or infinite).
##
## @code{integral3} is a wrapper for @code{triplequad} intended to provide
## @sc{matlab} compatibility.  More control of the numerical integration may be
## achievable by calling the various quadrature functions directly.
##
## @var{f} is a function handle, inline function, or string containing the name
## of the function to evaluate.  The function @var{f} must be of the form
## @math{w = f(x,y,z)} where either @var{x} or @var{y} is a vector and the
## remaining inputs are scalars.  @var{f} should return a vector of the same
## length and orientation as the vector input @var{x} or @var{y}.
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
##
## @item
## @code{integral3} currently only integrates functions over rectangular
## volumes.  Implementing @var{ya} and @var{yb} as functions of @var{x}, and
## @var{za} and @var{zb} as functions of (@var{x,y}) is a planned future
## improvement.
##
## @item
## The @qcode{"Method"} property is not yet implemented in Octave due to the
## lack of a @qcode{"tiled"} integrator implementation.  All integrals are
## evaluated using an equivalent of the @qcode{"iterated"} method.
## @end enumerate
##
## @seealso{integral, integral2, quad, quadgk, quadv, quadl, quadcc, trapz,
##          dblquad, triplequad}
## @end deftypefn

function q = integral3 (f, xa, xb, ya, yb, za, zb, varargin)
  ## FIXME: it is possible that a non-rectangular domain could be handled by
  ##        overlaying the integrand with a boolean mask function such that
  ##        the integration occurs over a rectangle but regions outside the
  ##        desired domain contribute zero to the integral. This may be an
  ##        inefficient but acceptable hack to get around the rectangular domain
  ##        limit without having to rewrite the integrating function.

  ## FIXME: implement 'method' property to let the user select between iterated
  ##        and tiled integration. Tiled integration follows the method of
  ##        matlab's quad2d function, currently unimplemented in Octave. Should
  ##        probably just wait for a quad2d implementation to point the
  ##        integral3 wrapper to instead of trying to recreate it here. The
  ##        following can be added to the help docstring once it is functional:
  ## @item Method
  ## Specifies the underlying 2D integration method to be used on the y and z
  ## dimensions, with valid options being @var{"auto"}, @var{"tiled"}, or
  ## @var{"iterated"}. @code{integral3} will use @var{"auto"} by default, where
  ## it will usually choose @var{"tiled"} unless any of the integration limits
  ## are infinite.

  if (nargin < 7 || (mod (nargin, 2) == 0))
    print_usage ();
  endif

  if (! is_function_handle (f))
    print_usage ();
  endif

  if (! (isscalar (xa) && isscalar (xb)
         && isscalar (ya) && isscalar (yb)
         && isscalar (za) && isscalar (zb)))
    print_usage ();
  endif

  ## Check for single or double limits to set appropriate default tolerance.
  issingle = isa ([xa, xb, ya, yb, za, zb], "single");

  ## Set defaults, update with any specified parameters.
  if (issingle)
    abstol = 1e-5;
    reltol = 1e-4;
  else
    abstol = 1e-10;
    reltol = 1e-6;
  endif

  if (nargin == 7)
    ## Pass the simplest case directly to integrator.
    ## Let quadcc function handle input checks.
    q = triplequad (f, xa, xb, ya, yb, za, zb, [abstol, reltol], @quadcc);

  else
    ## Parse options to determine how to call integrator.
    intmethod = [];

    idx = 1;
    while (idx < nargin - 7)
      prop = varargin{idx++};
      if (! ischar (prop))
        error ("integral3: property PROP must be a string");
      endif

      switch (tolower (prop))
        case "abstol"
          abstol = varargin{idx++};
          if (! (isnumeric (abstol) && isscalar (abstol) && abstol >= 0))
            error ("integral3: AbsTol value must be a numeric scalar >= 0");
          endif

        case "reltol"
          reltol = varargin{idx++};
          if (! (isnumeric (reltol) && isscalar (reltol) && reltol >= 0))
            error ("integral2: RelTol value must be a numeric scalar >= 0");
          endif

        case "method"
          intmethod = varargin{idx++};
          warning (["integral3: Only 'iterated' method implemented.  ", ...
                    "Method property ignored."]);
        otherwise
          error ("integral3: unknown property '%s'", prop);
      endswitch
    endwhile

    q = triplequad (f, xa, xb, ya, yb, za, zb, [abstol, reltol], @quadcc);

  endif

endfunction


%!test
%! f = @(x, y, z) x.*y.*z;
%! assert (integral3 (f, 0, 1, 0, 1, 0, 1), 0.125, 1e-10);

%!test
%! f = @(x,y,z) y.*sin(x) + z.*cos(x);
%! assert (integral3 (f, 0, pi, 0, 1, -1, 1), 2, 1e-10);

## tests from triplequad
%! assert (integral3 (@(x,y,z) exp (-x.^2 - y.^2 - z.^2) , -1, 1, -1, 1, -1,
%!         1), pi^(3/2) * erf (1).^3, 1e-10);

## Test input validation
%!error integral3 (0, 1 ,2 ,3 ,4, 5, 6)
%!error integral3 (@plus)
%!error integral3 (@plus, 1)
%!error integral3 (@plus, 1, 2)
%!error integral3 (@plus, 1, 2, 3)
%!error integral3 (@plus, 1, 2, 3, 4)
%!error integral3 (@plus, 1, 2, 3, 4, 5)
%!error integral3 (@plus, 1, 2, 3, 4, 5, [6 7])
%!error integral3 (@plus, 1, 2, 3, 4, 5, "test")
%!error integral3 (@plus, 1, 2, 3, 4, 5, 6, "foo")
%!error <unknown property 'foo'> integral3 (@plus, 1, 2, 3, 4, 5, 6, "foo", "bar")
%!error <property PROP must be a string> integral3 (@plus, 1, 2, 3, 4, 5, 6, NA, "bar")
%!error <AbsTol value must be a numeric> integral3 (@plus, 1, 2, 3, 4, 5, 6, "AbsTol", "foo")
%!error <AbsTol value must be a .* scalar> integral3 (@plus, 1, 2, 3, 4, 5, 6, "AbsTol", [1, 2])
%!error <AbsTol value must be.* .= 0> integral3 (@plus, 1, 2, 3, 4, 5, 6, "AbsTol", -1)
%!error <RelTol value must be a numeric> integral3 (@plus, 1, 2, 3, 4, 5, 6, "RelTol", "foo")
%!error <RelTol value must be a .* scalar> integral3 (@plus, 1, 2, 3, 4, 5, 6, "RelTol", [1, 2])
%!error <RelTol value must be.* .= 0> integral3 (@plus, 1, 2, 3, 4, 5, 6, "RelTol", -1)
%!warning <Only 'iterated' method implemented>
%! q = integral3 (@plus, 0, 1, 0, 1, 0, 1, "Method", "tiled");
