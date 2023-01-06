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
## @deftypefn  {} {@var{q} =} quad2d (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb})
## @deftypefnx {} {@var{q} =} quad2d (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {[@var{q}, @var{err}, @var{iter}] =} quad2d (@dots{})
##
## Numerically evaluate the two-dimensional integral of @var{f} using adaptive
## quadrature over the two-dimensional domain defined by @var{xa}, @var{xb},
## @var{ya}, @var{yb} using tiled integration.  Additionally, @var{ya} and
## @var{yb} may be scalar functions of @var{x}, allowing for the integration
## over non-rectangular domains.
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
## @item MaxFunEvals
## The maximum number of function calls to the vectorized function @var{f}.
## The default value is 5000.
##
## @item Singular
## Enable/disable transforms to weaken singularities on the edge of the
## integration domain.  The default value is @var{true}.
##
## @item Vectorized
## Option to disable vectorized integration, forcing Octave to use only scalar
## inputs when calling the integrand.  The default value is @var{false}.
##
## @item FailurePlot
## If @code{quad2d} fails to converge to the desired error tolerance before
## MaxFunEvals is reached, a plot of the areas that still need refinement
## is created.  The default value is @var{false}.
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
## The optional output @var{err} is an approximate bound on the error in the
## integral @code{abs (@var{q} - @var{I})}, where @var{I} is the exact value
## of the integral.  The optional output @var{iter} is the number of vectorized
## function calls to the function @var{f} that were used.
##
## Example 1 : integrate a rectangular region in x-y plane
##
## @example
## @group
## @var{f} = @@(@var{x},@var{y}) 2*ones (size (@var{x}));
## @var{q} = quad2d (@var{f}, 0, 1, 0, 1)
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
## @var{q} = quad2d (@var{f}, 0, 1, 0, @var{ymax})
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
## @seealso{integral2, dblquad, integral, quad, quadgk, quadv, quadl, quadcc,
##          trapz, integral3, triplequad}
## @end deftypefn

function [q, err, iter] = quad2d (f, xa, xb, ya, yb, varargin)

  if (nargin < 5 || mod (nargin, 2) == 0)
    print_usage ();
  endif

  if (ischar (f))
    ## Convert function given as a string to a function handle
    f = @(x) feval (f, x);
  elseif (! is_function_handle (f))
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

  vectorized = true;
  singular = true;
  idx = 1;
  maxiter = 5000;
  failureplot = false;

  while (idx < nargin - 5)
    prop = varargin{idx++};
    if (! ischar (prop))
      error ("quad2d: property PROP must be a string");
    endif

    switch (tolower (prop))
      case "abstol"
        abstol = varargin{idx++};
        if (! (isnumeric (abstol) && isscalar (abstol) && abstol >= 0))
          error ("quad2d: AbsTol value must be a numeric scalar >= 0");
        endif

      case "reltol"
        reltol = varargin{idx++};
        if (! (isnumeric (reltol) && isscalar (reltol) && reltol >= 0))
          error ("quad2d: RelTol value must be a numeric scalar >= 0");
        endif

      case "maxfunevals"
        maxiter = varargin{idx++};
        if (! (isnumeric (maxiter) && isscalar (maxiter)
               && fix (maxiter) == maxiter && maxiter >= 1))
          error ("quad2d: MaxFunEvals value must be a scalar integer >= 1");
        endif

      case "singular"
        singular = varargin{idx++};
        if (! (isscalar (singular) && isreal (singular)))
          error ("quad2d: Singular must be a logical value");
        endif

      case "vectorized"
        vectorized = varargin{idx++};
        if (! (isscalar (vectorized) && isreal (vectorized)))
          error ("quad2d: Vectorized must be a logical value");
        endif

      case "failureplot"
        failureplot = varargin{idx++};
        if (! (isscalar (failureplot) && isreal (failureplot)))
          error ("quad2d: FailurePlot must be a logical value");
        endif

      otherwise
        error ("quad2d: unknown property '%s'", prop);

    endswitch
  endwhile

  if (! vectorized)
    f = @(x, y) arrayfun (f, x, y);
  endif

  ## check upper and lower bounds of y
  if (! is_function_handle (ya))
    if (! (isreal (ya) && isscalar (ya)))
      error ("quad2d: YA must be a real scalar or a function");
    endif
    ya = @(x) ya * ones (rows (x), columns (x));
  endif
  if (! is_function_handle (yb))
    if (! (isreal (yb) && isscalar (yb)))
      error ("quad2d: YB must be a real scalar or a function");
    endif
    yb = @(x) yb * ones (rows (x), columns (x));
  endif

  iter = 0;
  qaccept = 0;
  qerraccept = 0;

  if (singular)
    ## Shampine suggests using the singularity weakening transform
    ## suggested by Havie.
    ##   \int_a^b f(x) dx = \int_0^pi f (g(t)) (dx / dt) dt
    ## where
    ##   g(t) = ((a - b) * cos(t) + (a + b)) / 2
    ##   dx = - (a - b) * sin(t) / 2 dt
    ## Now our integral is
    ##   \int_a^b \int_0^1 f(x,y) dydx
    ## as we already substitute for "y", so
    ##   gx(tx) = ((a - b) * cos(tx) + (a + b)) / 2
    ##   gy(ty) = (1 - cos(ty)) / 2
    ##   dydx = (b - a) * sin(tx) * sin(ty) / 4 dtydtx

    xtrans = @(tx) ((xa - xb) .* cos (tx) + (xa + xb)) ./ 2;
    ytrans = @(ty) (1 - cos (ty)) ./ 2;
    ztrans = @(tx, ty) (xb - xa) .* sin (tx) .* sin (ty) ./ 4;
    area = pi ^ 2;

    ## Initialize tile list
    tilelist(1) = struct ("xa", 0, "xb", pi, "ya", 0, "yb", pi, ...
                          "q", 0, "qerr", Inf);
  else
    xtrans = @(tx) tx;
    ytrans = @(ty) ty;
    ztrans = @(tx, ty)  1;
    area = (xb - xa);

    ## Initialize tile list
    tilelist(1) = struct ("xa", xa, "xb", xb, "ya", 0, "yb", 1, ...
                          "q", 0, "qerr", Inf);
  endif

  while (length (tilelist) > 0 && iter < maxiter)
    ## Get tile with the largest error
    [~, idx] = max ([tilelist.qerr]);
    tile = tilelist(idx);
    tilelist(idx) = [];

    ## Subdivide the tile into 4 subtiles
    iter += 4;
    tiles(4) = struct ("xa", tile.xa, "xb", tile.xa + (tile.xb - tile.xa) / 2,
                       "ya", tile.ya, "yb", tile.ya + (tile.yb - tile.ya) / 2,
                       "q", 0, "qerr", 0);
    tiles(3) = struct ("xa", tile.xa, "xb", tile.xa + (tile.xb - tile.xa) / 2,
                       "ya", tile.ya + (tile.yb - tile.ya) / 2, "yb", tile.yb,
                       "q", 0, "qerr", 0);
    tiles(2) = struct ("xa", tile.xa + (tile.xb - tile.xa) / 2, "xb", tile.xb,
                       "ya", tile.ya, "yb", tile.ya + (tile.yb - tile.ya) / 2,
                       "q", 0, "qerr", 0);
    tiles(1) = struct ("xa", tile.xa + (tile.xb - tile.xa) / 2, "xb", tile.xb,
                       "ya", tile.ya + (tile.yb - tile.ya) / 2, "yb", tile.yb,
                       "q", 0, "qerr", 0);

    ## Perform the quadrature of 4 subtiles
    for i = 1:4
      [tiles(i).q, tiles(i).qerr] = ...
        tensorproduct (f, ya, yb, tiles(i), xtrans, ytrans, ztrans, singular);
    endfor

    q = qaccept + sum ([[tilelist.q], tiles.q]);
    err = qerraccept + sum ([[tilelist.qerr], tiles.qerr]);
    tol = max (abstol, reltol .* abs (q));

    ## Shampine suggests taking a margin of a factor of 8 for
    ## the local tolerance.  That, and the fact that we are subdividing
    ## into 4 tiles, means we divide by 32 at this point.
    localtol = tol * ([tile.xb] - [tile.xa]) * ([tile.yb] - [tile.ya]) ...
               / area / 32;

    ## If global tolerance is met, return.
    if (err < tol)
      break;
    endif

    ## Accept the tiles meeting the tolerance, and add the others back to
    ## the list of tiles to treat.
    idx = find ([tiles.qerr] < localtol);
    qaccept += sum ([tiles(idx).q]);
    qerraccept += sum ([tiles(idx).qerr]);
    tiles(idx) = [];
    tilelist = [tilelist, tiles];
  endwhile

  ## Verify convergence
  if (iter >= maxiter)
    if (err > max (abstol, reltol .* abs (q)))
      warning (["quad2d: " ...
                "Maximum number of sub-tiles reached without convergence"]);
    else
      warning (["quad2d: " ...
                "Maximum number of sub-tiles reached, accuracy may be low"]);
    endif
    if (failureplot)
      newplot ();
      title ("quad2d : Areas needing refinement");
      for tile = tilelist
        xaa = xtrans(tile.xa);
        xbb = xtrans(tile.xb);
        y1 = ya(xaa) + ytrans(tile.ya) * (yb(xaa) - ya(xaa));
        y2 = ya(xaa) + ytrans(tile.yb) * (yb(xaa) - ya(xaa));
        y3 = ya(xbb) + ytrans(tile.yb) * (yb(xbb) - ya(xbb));
        y4 = ya(xbb) + ytrans(tile.ya) * (yb(xbb) - ya(xbb));
        patch ([xaa, xaa, xbb, xbb, xaa], [y1, y2, y3, y4, y1], "b");
      endfor
    endif
  endif

endfunction

function [q, qerr] = tensorproduct (f, ya, yb, tile, xtrans, ytrans, ztrans, singular)

  ## The Shampine TwoD paper proposes using a G3,K7 rule in a tensor product.
  ## I couldn't find a tabulated abscissas and weights of a G3,K7 rule publicly
  ## available, so use a G7,K15 rule from Octave's implementation of quadgk.

  persistent abscissa = [-0.9914553711208126e+00, -0.9491079123427585e+00, ...
                         -0.8648644233597691e+00, -0.7415311855993944e+00, ...
                         -0.5860872354676911e+00, -0.4058451513773972e+00, ...
                         -0.2077849550078985e+00,  0.0000000000000000e+00, ...
                          0.2077849550078985e+00,  0.4058451513773972e+00, ...
                          0.5860872354676911e+00,  0.7415311855993944e+00, ...
                          0.8648644233597691e+00,  0.9491079123427585e+00, ...
                          0.9914553711208126e+00];

  persistent weights15 = [0.2293532201052922e-01,  0.6309209262997855e-01, ...
                          0.1047900103222502e+00,  0.1406532597155259e+00, ...
                          0.1690047266392679e+00,  0.1903505780647854e+00, ...
                          0.2044329400752989e+00,  0.2094821410847278e+00, ...
                          0.2044329400752989e+00,  0.1903505780647854e+00, ...
                          0.1690047266392679e+00,  0.1406532597155259e+00, ...
                          0.1047900103222502e+00,  0.6309209262997855e-01, ...
                          0.2293532201052922e-01];

  persistent weights7  = [0.0, ...
                          0.1294849661688697e+00, 0.0, ...
                          0.2797053914892767e+00, 0.0, ...
                          0.3818300505051889e+00, 0.0, ...
                          0.4179591836734694e+00, 0.0, ...
                          0.3818300505051889e+00, 0.0, ...
                          0.2797053914892767e+00, 0.0, ...
                          0.1294849661688697e+00, 0.0];

  xaa = tile.xa;
  xbb = tile.xb;
  yaa = tile.ya;
  ybb = tile.yb;

  tx = ((xbb - xaa) * abscissa + xaa + xbb) / 2;
  x = xtrans(tx);
  ty = (abscissa' * (ybb - yaa)  + yaa + ybb) / 2;
  y = ones (15, 1) * ya(x) + ytrans(ty) * (yb(x) - ya(x));

  xhalfwidth = (xbb - xaa ) / 2;
  yhalfwidth = ones (15, 1) * (yb(x) - ya(x)) .* (ybb - yaa) ./ 2;

  x = ones (15, 1) * x;
  tx = ones (15,1) * tx;
  ty = ty * ones (1, 15);

  z = yhalfwidth .* f (x, y) .* ztrans(tx, ty) .* xhalfwidth;
  q = weights15 * (weights15 * z).';
  qerr = abs (weights7 * (weights7 * z).' - q);

endfunction


%!shared f
%! f = @(x, y) x .* y;
%!assert (quad2d (f, 0, 1, 0, 1), 0.25, 1e-10)

%!test
%! f = @(x, y) 9 * x.^2 + 15 * y.^2;
%!assert (quad2d (f, 0, 5, -5, 0, "AbsTol", 1e-9), 5000, 1e-9)
%!assert (quad2d (f, 0, 5, -5, 0, "RelTol", 1e-6), 5000, -1e-6)
%!assert (quad2d (f, 0, 5, -5, 0, "RelTol", 1e-6, "AbsTol", 1e-9), 5000, 1e-9)

## tests from dblquad
%!test
%! f = @(x, y) 1 ./ (x+y);
%!assert (quad2d (f, 0, 1, 0, 1, "AbsTol", 1e-7), 2*log (2), 1e-7)
%!assert (quad2d (f, 0, 1, 0, 1, "RelTol", 1e-5), 2*log (2), -1e-5)
%!assert (quad2d (f, 0, 1, 0, 1, "AbsTol", 1e-8, "RelTol", 1e-6),
%!        2*log (2), -1e-6)
%!assert (quad2d (f, 0, 1, 0, @(x) 1 - x), 1, -1e-6)
%!assert (quad2d (f, 0, 1, 0, @(x) 1 - x, "Singular", true), 1, -1e-6)

%!assert (quad2d (@(x, y) exp (-x.^2 - y.^2) , -1, 1, -1, 1),
%!        pi * erf (1).^2, 1e-10)

%!assert (quad2d (@plus, 1, 2, 3, 4), 5, 1e-10)

%!assert <*62972> (quad2d (@(x,y) 1i*ones (size (x)), 0,1,0,1), 1i)

## Test input validation
%!error <Invalid call> quad2d ()
%!error <Invalid call> quad2d (@plus)
%!error <Invalid call> quad2d (@plus, 1)
%!error <Invalid call> quad2d (@plus, 1, 2)
%!error <Invalid call> quad2d (@plus, 1, 2, 3)
%!error <Invalid call> quad2d (@plus, 1, 2, 3, 4, "foo")
%!error quad2d (0, 1, 2, 3, 4)          # f must be function handle
%!error quad2d (@plus, 1i, 2, 3, 4)     # real limits
%!error quad2d (@plus, 1, 2i, 3, 4)     # real limits
%!error quad2d (@plus, [1 1], 2, 3, 4)  # scalar limits
%!error quad2d (@plus, 1, [2 2], 3, 4)  # scalar limits
%!error <property PROP must be a string> quad2d (@plus, 1, 2, 3, 4, 99, "bar")
%!error <AbsTol value must be a numeric> quad2d (@plus, 1, 2, 3, 4, "AbsTol", "foo")
%!error <AbsTol value must be a .* scalar> quad2d (@plus, 1, 2, 3, 4, "AbsTol", [1, 2])
%!error <AbsTol value must be.* .= 0> quad2d (@plus, 1, 2, 3, 4, "AbsTol", -1)
%!error <RelTol value must be a numeric> quad2d (@plus, 1, 2, 3, 4, "RelTol", "foo")
%!error <RelTol value must be a .* scalar> quad2d (@plus, 1, 2, 3, 4, "RelTol", [1, 2])
%!error <RelTol value must be.* .= 0> quad2d (@plus, 1, 2, 3, 4, "RelTol", -1)
%!error <MaxFunEvals value must be a scalar integer>
%! quad2d (@plus,1,2,3,4, "MaxFunEvals", {1});
%!error <MaxFunEvals value must be a scalar integer>
%! quad2d (@plus,1,2,3,4, "MaxFunEvals", [1 1]);
%!error <MaxFunEvals value must be a scalar integer>
%! quad2d (@plus,1,2,3,4, "MaxFunEvals", 1.5);
%!error <MaxFunEvals value must be a scalar integer .= 1>
%! quad2d (@plus,1,2,3,4, "MaxFunEvals", -1);
%!error <Singular must be a logical value>
%! quad2d (@plus,1,2,3,4, "Singular", [0 1]);
%!error <Singular must be a logical value>
%! quad2d (@plus,1,2,3,4, "Singular", {true});
%!error <Vectorized must be a logical value>
%! quad2d (@plus,1,2,3,4, "Vectorized", [0 1]);
%!error <Vectorized must be a logical value>
%! quad2d (@plus,1,2,3,4, "Vectorized", {true});
%!error <FailurePlot must be a logical value>
%! quad2d (@plus,1,2,3,4, "FailurePlot", [0 1]);
%!error <FailurePlot must be a logical value>
%! quad2d (@plus,1,2,3,4, "FailurePlot", {true});
%!error <unknown property 'foo'>  quad2d (@plus, 1, 2, 3, 4, "foo", "bar")
%!error <YA must be a real scalar> quad2d (@plus, 1, 2, 3i, 4)
%!error <YA must be a real scalar> quad2d (@plus, 1, 2, [3 3], 4)
%!error <YB must be a real scalar> quad2d (@plus, 1, 2, 3, 4i)
%!error <YB must be a real scalar> quad2d (@plus, 1, 2, 3, [4 4])
%!warning <Maximum number of> quad2d (@plus, 1, 2, 3, 4, "MaxFunEvals", 1);
