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
## @deftypefn  {} {@var{q} =} integral2 (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb})
## @deftypefnx {} {@var{q} =} integral2 (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {[@var{q}, @var{err}] =} integral2 (@dots{})
##
## Numerically evaluate the two-dimensional integral of @var{f} using adaptive
## quadrature over the two-dimensional domain defined by @var{xa}, @var{xb},
## @var{ya}, @var{yb} (scalars may be finite or infinite).  Additionally,
## @var{ya} and @var{yb} may be scalar functions of @var{x}, allowing for
## integration over non-rectangular domains.
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
## Example 1 : integrate a rectangular region in x-y plane
##
## @example
## @group
## @var{f} = @@(@var{x},@var{y}) 2*ones (size (@var{x}));
## @var{q} = integral2 (@var{f}, 0, 1, 0, 1)
##   @result{} @var{q} =  2
## @end group
## @end example
##
## The result is a volume, which for this constant-value integrand, is just
## @code{@var{Length} * @var{Width} * @var{Height}}.
##
## Example 2 : integrate a triangular region in x-y plane
##
## @example
## @group
## @var{f} = @@(@var{x},@var{y}) 2*ones (size (@var{x}));
## @var{ymax} = @@(@var{x}) 1 - @var{x};
## @var{q} = integral2 (@var{f}, 0, 1, 0, @var{ymax})
##   @result{} @var{q} =  1
## @end group
## @end example
##
## The result is a volume, which for this constant-value integrand, is the
## Triangle Area x Height or
## @code{1/2 * @var{Base} * @var{Width} * @var{Height}}.
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
## @seealso{quad2d, dblquad, integral, quad, quadgk, quadv, quadl, quadcc,
##          trapz, integral3, triplequad}
## @end deftypefn

function [q, err] = integral2 (f, xa, xb, ya, yb, varargin)

  if (nargin < 5 || mod (nargin, 2) == 0)
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
              || (! is_function_handle (yb) && isa (yb, "single")));

  ## Set defaults, update with any specified parameters.
  if (issingle)
    abstol = 1e-5;
    reltol = 1e-4;
  else
    abstol = 1e-10;
    reltol = 1e-6;
  endif

  method = "auto";
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
        method = tolower (varargin{idx++});
        if (! any (strcmp (method, {"auto", "iterated", "tiled"})))
          error ("integral2 : unrecognized method '%s'", method);
        endif

      case "vectorized"
        vectorized = varargin{idx++};
        if (! (isscalar (vectorized) && isreal (vectorized)))
          error ('integral2: Vectorized must be a logical value');
        endif
        if (! vectorized)
          f = @(x, y) arrayfun (f, x, y);
        endif

      otherwise
        error ("integral2: unknown property '%s'", prop);

    endswitch
  endwhile

  if (strcmp (method, "auto"))
    if (isinf (xa) || isinf (xb)
        || (! is_function_handle (ya) && isinf (ya))
        || (! is_function_handle (yb) && isinf (yb)))
      method = "iterated";
    else
      method = "tiled";
    endif
  endif

  ## check upper and lower bounds of y
  if (! is_function_handle (ya))
    if (! (isreal (ya) && isscalar (ya)))
      error ("integral2: YA must be a real scalar or a function");
    endif
    ya = @(x) ya * ones (rows (x), columns (x));
  endif
  if (! is_function_handle (yb))
    if (! (isreal (yb) && isscalar (yb)))
      error ("integral2: YB must be a real scalar or a function");
    endif
    yb = @(x) yb * ones (rows (x), columns (x));
  endif

  if (strcmp (method, "iterated"))
    q = outer_iterated (f, xa, xb, ya, yb, abstol, reltol);

    if (nargout == 2)
      warning ('integral2: "iterated" method can not return estimated error');
      err = 0;
    endif
  else
    [q, err] = quad2d (f, xa, xb, ya, yb, "AbsTol", abstol, "RelTol", reltol);
  endif

endfunction

function q = outer_iterated (f, xa, xb, ya, yb, abstol, reltol)
  finner_iter = @(x) inner_iterated (x, f, ya, yb, abstol, reltol);
  q = quadcc (finner_iter, xa, xb, [abstol, reltol]);
endfunction

function q = inner_iterated (x, f, ya, yb, abstol, reltol)
  q = zeros (size (x));
  for i = 1 : length (x)
    q(i) = quadcc (@(y) f(x(i), y), ya(x(i)), yb(x(i)), [abstol, reltol]);
  endfor
endfunction


## method tests
%!shared f
%! f = @(x, y) x .* y;

%!assert (integral2 (f, 0, 1, 0, 1), 0.25, 1e-10)
%!assert (integral2 (f, 0, 1, 0, 1, "method", "tiled"), 0.25, 1e-10)
%!assert (integral2 (f, 0, 1, 0, 1, "method", "iterated"), 0.25, 1e-10)
%!assert (integral2 (f, 0, 1, 0, 1, "method", "auto"), 0.25, 1e-10)

## vectorized = false test
%!test
%! f = @(x, y) x * y;
%!assert (integral2 (f, 0, 1, 0, 1, "vectorized", false), 0.25, 1e-10)

## tolerance tests
%!test
%! f = @(x, y) 9 * x.^2 + 15 * y.^2;
%!assert (integral2 (f, 0, 5, -5, 0, "AbsTol", 1e-9), 5000, 1e-9)
%!assert (integral2 (f, 0, 5, -5, 0, "RelTol", 1e-5), 5000, -1e-5)
%!assert (integral2 (f, 0, 5, -5, 0, "RelTol", 1e-6, "AbsTol", 1e-9),
%!        5000, 1e-9)

## tests from dblquad
%!test
%! f = @(x, y) 1 ./ (x+y);
%!assert (integral2 (f, 0, 1, 0, 1, "AbsTol", 1e-7), 2*log (2), 1e-7)
%!assert (integral2 (f, 0, 1, 0, 1, "RelTol", 1e-5), 2*log (2), -1e-5)
%!assert (integral2 (f, 0, 1, 0, 1, "AbsTol", 1e-8, "RelTol", 1e-6),
%!        2*log (2), -1e-6)
%!assert (integral2 (f, 0, 1, 0, @(x) 1 - x), 1, -1e-6)

%!assert (integral2 (@(x, y) exp (-x.^2 - y.^2) , -1, 1, -1, 1),
%!        pi * erf (1).^2, 1e-10)

%!assert (integral2 (@plus, 1, 2, 3, 4), 5, 1e-10)

## tests from dblquad w/method specified
%!assert (integral2 (f, 0, 1, 0, 1, "AbsTol", 1e-7, "method", "iterated"),
%!        2*log (2), 1e-7)
%!assert (integral2 (f, 0, 1, 0, 1, "RelTol", 1e-5, "method", "iterated"),
%!        2*log (2), -1e-5)
%!assert (integral2 (f, 0, 1, 0, 1, "AbsTol", 1e-8, "RelTol", 1e-6,
%!                                  "Method", "iterated"),
%!        2*log (2), -1e-6)
%!assert (integral2 (f, 0, 1, 0, @(x) 1 - x, "Method", "iterated"), 1, -1e-6)
%!assert (integral2 (@(x, y) exp (-x.^2 - y.^2) , -1, 1, -1, 1,
%!                                                "Method", "iterated"),
%!        pi * erf (1).^2, 1e-10)

%!assert (integral2 (@plus, 1, 2, 3, 4, "method", "iterated"), 5, 1e-10)

## Test input validation
%!error <Invalid call> integral2 ()
%!error <Invalid call> integral2 (@plus)
%!error <Invalid call> integral2 (@plus, 1)
%!error <Invalid call> integral2 (@plus, 1, 2)
%!error <Invalid call> integral2 (@plus, 1, 2, 3)
%!error <Invalid call> integral2 (@plus, 1, 2, 3, 4, "foo")
%!error integral2 (0, 1, 2, 3, 4)          # f must be function handle
%!error integral2 (@plus, 1i, 2, 3, 4)     # real limits
%!error integral2 (@plus, 1, 2i, 3, 4)     # real limits
%!error integral2 (@plus, [1 1], 2, 3, 4)  # scalar limits
%!error integral2 (@plus, 1, [2 2], 3, 4)  # scalar limits
%!error <property PROP must be a string> integral2 (@plus,1,2,3,4,99, "bar")
%!error <AbsTol value must be a numeric>
%! integral2 (@plus,1,2,3,4, "AbsTol", "foo");
%!error <AbsTol value must be a .* scalar>
%! integral2 (@plus, 1, 2, 3, 4, "AbsTol", [1, 2]);
%!error <AbsTol value must be.* .= 0> integral2 (@plus,1,2,3,4, "AbsTol", -1)
%!error <RelTol value must be a numeric>
%! integral2 (@plus, 1, 2, 3, 4, "RelTol", "foo");
%!error <RelTol value must be a .* scalar>
%! integral2 (@plus, 1, 2, 3, 4, "RelTol", [1, 2]);
%!error <RelTol value must be.* .= 0> integral2 (@plus,1,2,3,4, "RelTol", -1)
%!error <unrecognized method 'foo'> integral2 (@plus,1,2,3,4, "method", "foo")
%!error <Vectorized must be a logical value>
%! integral2 (@plus,1,2,3,4, "Vectorized", [0 1]);
%!error <Vectorized must be a logical value>
%! integral2 (@plus,1,2,3,4, "Vectorized", {true});
%!error <unknown property 'foo'>  integral2 (@plus, 1, 2, 3, 4, "foo", "bar")
%!error <YA must be a real scalar> integral2 (@plus, 1, 2, 3i, 4)
%!error <YA must be a real scalar> integral2 (@plus, 1, 2, [3 3], 4)
%!error <YB must be a real scalar> integral2 (@plus, 1, 2, 3, 4i)
%!error <YB must be a real scalar> integral2 (@plus, 1, 2, 3, [4 4])
%!warning <"iterated" method can not return estimated error>
%! [q, err] = integral2 (@plus, 0, 0, 0, 0, "method", "iterated");
