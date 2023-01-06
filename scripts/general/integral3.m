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
## @deftypefn  {} {@var{q} =} integral3 (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{za}, @var{zb})
## @deftypefnx {} {@var{q} =} integral3 (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{za}, @var{zb}, @var{prop}, @var{val}, @dots{})
##
## Numerically evaluate the three-dimensional integral of @var{f} using
## adaptive quadrature over the three-dimensional domain defined by
## @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{za}, @var{zb} (scalars may
## be finite or infinite).  Additionally, @var{ya} and @var{yb} may be
## scalar functions of @var{x} and @var{za}, and @var{zb} maybe be scalar
## functions of @var{x} and @var{y}, allowing for integration over
## non-rectangular domains.
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
##
## @item Method
## Specify the two-dimensional integration method to be used, with valid
## options being @qcode{"auto"} (default), @qcode{"tiled"}, or
## @qcode{"iterated"}.  When using @qcode{"auto"}, Octave will choose the
## @qcode{"tiled"} method unless any of the integration limits are infinite.
##
## @item Vectorized
## Enable or disable vectorized integration.  A value of @code{false} forces
## Octave to use only scalar inputs when calling the integrand, which enables
## integrands @math{f(x,y)} that have not been vectorized and only accept
## @var{x} and @var{y} as scalars to be used.  The default value is
## @code{true}.
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
##         @var{error} <= max (@var{AbsTol}, @var{RelTol}*|@var{q}|)
## @end group
## @end example
##
## @end ifnottex
##
## @var{err} is an approximate bound on the error in the integral
## @code{abs (@var{q} - @var{I})}, where @var{I} is the exact value of the
## integral.
##
## Example 1 : integrate over a rectangular volume
##
## @example
## @group
## @var{f} = @@(@var{x},@var{y},@var{z}) ones (size (@var{x}));
## @var{q} = integral3 (@var{f}, 0, 1, 0, 1, 0, 1)
##   @result{} @var{q} =  1.00000
## @end group
## @end example
##
## For this constant-value integrand, the result is a volume which is just
## @code{@var{Length} * @var{Width} * @var{Height}}.
##
## Example 2 : integrate over a spherical volume
##
## @example
## @group
## @var{f} = @@(@var{x},@var{y}) ones (size (@var{x}));
## @var{ymax} = @@(@var{x}) sqrt (1 - @var{x}.^2);
## @var{zmax} = @@(@var{x},@var{y}) sqrt (1 - @var{x}.^2 - @var{y}.^2);
## @var{q} = integral3 (@var{f}, 0, 1, 0, @var{ymax}, 0, @var{zmax})
##   @result{} @var{q} =  0.52360
## @end group
## @end example
##
## For this constant-value integrand, the result is a volume which is 1/8th
## of a unit sphere or @code{1/8 * 4/3 * pi}.
##
## Programming Notes: If there are singularities within the integration region
## it is best to split the integral and place the singularities on the
## boundary.
##
## Known @sc{matlab} incompatibility: If tolerances are left unspecified, and
## any integration limits are of type @code{single}, then Octave's integral
## functions automatically reduce the default absolute and relative error
## tolerances as specified above.  If tighter tolerances are desired they
## must be specified.  @sc{matlab} leaves the tighter tolerances appropriate
## for @code{double} inputs in place regardless of the class of the
## integration limits.
##
## Reference: @nospell{L.F. Shampine},
## @cite{@sc{matlab} program for quadrature in 2D}, Applied Mathematics and
## Computation, pp.@: 266--274, Vol 1, 2008.
##
## @seealso{triplequad, integral, quad, quadgk, quadv, quadl,
##          quadcc, trapz, integral2, quad2d, dblquad}
## @end deftypefn

function q = integral3 (f, xa, xb, ya, yb, za, zb, varargin)

  if (nargin < 7 || mod (nargin, 2) == 0)
    print_usage ();
  endif

  if (! is_function_handle (f))
    print_usage ();
  endif

  if (! (isreal (xa) && isscalar (xa) && isreal (xb) && isscalar (xb)))
    print_usage ();
  endif

  ## Check for single or double limits to set appropriate default tolerance.
  issingle = (isa ([xa, xb], "single")
              || (! is_function_handle (ya) && isa (ya, "single"))
              || (! is_function_handle (yb) && isa (yb, "single"))
              || (! is_function_handle (za) && isa (za, "single"))
              || (! is_function_handle (zb) && isa (zb, "single")));

  ## Communicate to downstream quadrature routines that at least one limit of
  ## integration was of single type by casting xa, xb to single.
  if (issingle)
    xa = single (xa);
    xb = single (xb);
  endif

  ## Set default tolerances, and then update with any specified parameters.
  if (issingle)
    abstol = 1e-5;
    reltol = 1e-4;
  else
    abstol = 1e-10;
    reltol = 1e-6;
  endif

  method = "auto";
  vectorized = true;
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
          error ("integral3: RelTol value must be a numeric scalar >= 0");
        endif

      case "method"
        method = tolower (varargin{idx++});
        if (! any (strcmp (method, {"auto", "iterated", "tiled"})))
          error ("integral3 : unrecognized method '%s'", method);
        endif

      case "vectorized"
        vectorized = varargin{idx++};
        if (! (isscalar (vectorized) && isreal (vectorized)))
          error ('integral3: Vectorized must be a logical value');
        endif

      otherwise
        error ("integral3: unknown property '%s'", prop);

    endswitch
  endwhile

  if (strcmp (method, "auto"))
    if (isinf (xa) || isinf (xb)
        || (! is_function_handle (ya) && isinf (ya))
        || (! is_function_handle (yb) && isinf (yb))
        || (! is_function_handle (za) && isinf (za))
        || (! is_function_handle (zb) && isinf (zb)))
      method = "iterated";
    else
      method = "tiled";
    endif
  endif

  ## check upper and lower bounds of y
  if (! is_function_handle (ya))
    if (! (isreal (ya) && isscalar (ya)))
      error ("integral3: YA must be a real scalar or a function");
    endif
    ya = @(x) ya * ones (size (x));
  endif
  if (! is_function_handle (yb))
    if (! (isreal (yb) && isscalar (yb)))
      error ("integral3: YB must be a real scalar or a function");
    endif
    yb = @(x) yb * ones (size (x));
  endif

  ## check upper and lower bounds of z
  if (! is_function_handle (za))
    if (! (isreal (za) && isscalar (za)))
      error ("integral3: ZA must be a real scalar or a function");
    endif
    za = @(x, y) za * ones (size (y));
  endif
  if (! is_function_handle (zb))
    if (! (isreal (zb) && isscalar (zb)))
      error ("integral3: ZB must be a real scalar or a function");
    endif
    zb = @(x, y) zb * ones (size (y));
  endif

  finner = @(x) inner (x, f, ya, yb, za, zb, vectorized, method, abstol, reltol);
  q = quadcc (finner, xa, xb, [abstol, reltol]);

endfunction

function q = inner (x, f, ya, yb, za, zb, vectorized, method, abstol, reltol)

  q = zeros (size (x));
  for i = 1 : length (x)
    za2 = @(y) za(x(i), y);
    zb2 = @(y) zb(x(i), y);
    f2 = @(y, z) f(x(i), y, z);
    if (! vectorized)
      f2 = @(y, z) arrayfun (f2, y, z);
    endif
    if (strcmp (method, "iterated"))
      finner_iter = @(y) inner_iterated (y, f2, za2, zb2, abstol, reltol);
      q(i) = quadcc (finner_iter, ya(x(i)), yb(x(i)), [abstol, reltol]);
    else
      q(i) = quad2d (f2, ya(x(i)), yb(x(i)), za2, zb2,
                     "AbsTol", abstol, "RelTol", reltol);
    endif
  endfor

endfunction

function q = inner_iterated (y, f2, za2, zb2, abstol, reltol)
  q = zeros (size (y));
  for i = 1 : length (y)
    q(i) = quadcc (@(z) f2(y(i), z), za2(y(i)), zb2(y(i)), [abstol, reltol]);
  endfor
endfunction


## method tests
%!shared f
%! f = @(x, y, z) x .* y .* z;

%!assert (integral3 (f, 0, 1, 0, 1, 0, 1), 0.125, 1e-10)
%!assert (integral3 (f, 0, 1, 0, 1, 0, 1, "method", "tiled"), 0.125, 1e-10)
%!assert (integral3 (f, 0, 1, 0, 1, 0, 1, "method", "iterated"), 0.125, 1e-10)
%!assert (integral3 (f, 0, 1, 0, 1, 0, 1, "method", "auto"), 0.125, 1e-10)

## vectorized = false test
%!test
%! f = @(x, y, z) x * y * z;
%! assert (integral3 (f, 0, 1, 0, 1, 0, 1, "vectorized", false), 0.125, 1e-10);

## tolerance tests
%!test
%! f = @(x, y, z) 2 * x.^2 + 3 * y.^2 + 4 * z.^2;
%!assert (integral3 (f, 0, 5, -5, 0, 0, 5, "AbsTol", 1e-9), 9375, 1e-9)
%!assert (integral3 (f, 0, 5, -5, 0, 0, 5, "RelTol", 1e-5), 9375, -1e-5)
%!assert (integral3 (f, 0, 5, -5, 0, 0, 5, "RelTol", 1e-6, "AbsTol", 1e-9),
%!        9375, 1e-9)

## non-rectangular region
## This test is too slow with "iterated" method
%!test
%! f = @(x,y,z) 1 ./ (x + y + z);
%! ymax = @(x) 1 - x;
%! zmax = @(x, y) 1 - x - y;
%! assert (integral3 (f, 0, 1, 0, ymax, 0, zmax, "method", "tiled"),
%!         0.25, 1e-6);

## Test input validation
%!error integral3
%!error integral3 (@plus)
%!error integral3 (@plus, 1)
%!error integral3 (@plus, 1, 2)
%!error integral3 (@plus, 1, 2, 3)
%!error integral3 (@plus, 1, 2, 3, 4)
%!error integral3 (@plus, 1, 2, 3, 4, 5)
%!error integral3 (@plus, 1, 2, 3, 4, 5, 6, "foo")
%!error integral3 (0, 1, 2, 3, 4, 5, 6)          # f must be a function handle
%!error integral3 (@plus, 1i, 2, 3, 4, 5, 6)     # real limits
%!error integral3 (@plus, 1, 2i, 3, 4, 5, 6)     # real limits
%!error integral3 (@plus, [1 1], 2, 3, 4, 5, 6)  # scalar limits
%!error integral3 (@plus, 1, [2 2], 3, 4, 5, 6)  # scalar limits
%!error <property PROP must be a string>
%! integral3 (@plus, 1, 2, 3, 4, 5, 6, 99, "bar");
%!error <AbsTol value must be a numeric>
%! integral3 (@plus, 1, 2, 3, 4, 5, 6, "AbsTol", "foo");
%!error <AbsTol value must be a .* scalar>
%! integral3 (@plus, 1, 2, 3, 4, 5, 6, "AbsTol", [1, 2]);
%!error <AbsTol value must be.* .= 0>
%! integral3 (@plus, 1, 2, 3, 4, 5, 6, "AbsTol", -1);
%!error <RelTol value must be a numeric>
%! integral3 (@plus, 1, 2, 3, 4, 5, 6, "RelTol", "foo");
%!error <RelTol value must be a .* scalar>
%! integral3 (@plus, 1, 2, 3, 4, 5, 6, "RelTol", [1, 2]);
%!error <RelTol value must be.* .= 0>
%! integral3 (@plus, 1, 2, 3, 4, 5, 6, "RelTol", -1);
%!error <unrecognized method 'foo'>
%! integral3 (@plus,1,2,3,4,5,6, "method", "foo");
%!error <Vectorized must be a logical value>
%! integral3 (@plus,1,2,3,4,5,6, "Vectorized", [0 1]);
%!error <Vectorized must be a logical value>
%! integral3 (@plus,1,2,3,4,5,6, "Vectorized", {true});
%!error <unknown property 'foo'>
%! integral3 (@plus, 1, 2, 3, 4, 6, 6, "foo", "bar");
%!error <YA must be a real scalar> integral3 (@plus, 1, 2, 3i, 4, 5, 6)
%!error <YA must be a real scalar> integral3 (@plus, 1, 2, [3 3], 4, 5, 6)
%!error <YB must be a real scalar> integral3 (@plus, 1, 2, 3, 4i, 5, 6)
%!error <YB must be a real scalar> integral3 (@plus, 1, 2, 3, [4 4], 5, 6)
%!error <ZA must be a real scalar> integral3 (@plus, 1, 2, 3, 4, 5i, 6)
%!error <ZA must be a real scalar> integral3 (@plus, 1, 2, 3, 4, [5 5], 6)
%!error <ZB must be a real scalar> integral3 (@plus, 1, 2, 3, 4, 5, 6i)
%!error <ZB must be a real scalar> integral3 (@plus, 1, 2, 3, 4, 5, [6 6])
