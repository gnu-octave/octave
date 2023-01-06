########################################################################
##
## Copyright (C) 1999-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{zi} =} griddata (@var{x}, @var{y}, @var{z}, @var{xi}, @var{yi})
## @deftypefnx {} {@var{zi} =} griddata (@var{x}, @var{y}, @var{z}, @var{xi}, @var{yi}, @var{method})
## @deftypefnx {} {[@var{xi}, @var{yi}, @var{zi}] =} griddata (@dots{})
## @deftypefnx {} {@var{vi} =} griddata (@var{x}, @var{y}, @var{z}, @var{v}, @var{xi}, @var{yi}, @var{zi})
## @deftypefnx {} {@var{vi} =} griddata (@var{x}, @var{y}, @var{z}, @var{v}, @var{xi}, @var{yi}, @var{zi}, @var{method})
## @deftypefnx {} {@var{vi} =} griddata (@var{x}, @var{y}, @var{z}, @var{v}, @var{xi}, @var{yi}, @var{zi}, @var{method}, @var{options})
##
## Interpolate irregular 2-D and 3-D source data at specified points.
##
## For 2-D interpolation, the inputs @var{x} and @var{y} define the points
## where the function @code{@var{z} = f (@var{x}, @var{y})} is evaluated.
## The inputs @var{x}, @var{y}, @var{z} are either vectors of the same length,
## or the unequal vectors @var{x}, @var{y} are expanded to a 2-D grid with
## @code{meshgrid} and @var{z} is a 2-D matrix matching the resulting size of
## the X-Y grid.
##
## The interpolation points are (@var{xi}, @var{yi}).  If, and only if,
## @var{xi} is a row vector and @var{yi} is a column vector, then
## @code{meshgrid} will be used to create a mesh of interpolation points.
##
## For 3-D interpolation, the inputs @var{x}, @var{y}, and @var{z} define the
## points where the function @code{@var{v} = f (@var{x}, @var{y}, @var{z})}
## is evaluated.  The inputs @var{x}, @var{y}, @var{z} are either vectors of
## the same length, or if they are of unequal length, then they are expanded to
## a 3-D grid with @code{meshgrid}.  The size of the input @var{v} must match
## the size of the original data, either as a vector or a matrix.
##
## The optional input interpolation @var{method} can be @qcode{"nearest"},
## @qcode{"linear"}, or for 2-D data @qcode{"v4"}.  When the method is
## @qcode{"nearest"}, the output @var{vi} will be the closest point in the
## original data (@var{x}, @var{y}, @var{z}) to the query point (@var{xi},
## @var{yi}, @var{zi}).  When the method is @qcode{"linear"}, the output
## @var{vi} will be a linear interpolation between the two closest points in
## the original source data in each dimension.  For 2-D cases only, the
## @qcode{"v4"} method is also available which implements a biharmonic spline
## interpolation.  If @var{method} is omitted or empty, it defaults to
## @qcode{"linear"}.
##
## For 3-D interpolation, the optional argument @var{options} is passed
## directly to Qhull when computing the Delaunay triangulation used for
## interpolation.  For more information on the defaults and how to pass
## different values, @pxref{XREFdelaunayn,,@code{delaunayn}}.
##
## Programming Notes: If the input is complex the real and imaginary parts
## are interpolated separately.  Interpolation is normally based on a
## Delaunay triangulation.  Any query values outside the convex hull of the
## input points will return @code{NaN}.  However, the @qcode{"v4"} method does
## not use the triangulation and will return values outside the original data
## (extrapolation).
## @seealso{griddata3, griddatan, delaunay}
## @end deftypefn

function [rx, ry, rz] = griddata (x, y, z, varargin)

  if (nargin < 5)
    print_usage ();
  endif

  if (nargin > 6)
    ## Current 2-D implementation has nargin max = 6, since no triangulation
    ## options are passed to the 2-D algorithm.
    ## 3-D algorithm requires nargin >=7.

    if (nargout > 1)
      error ("griddata: only one output argument valid for 3-D interpolation");
    endif
    rx = griddata3 (x, y, z, varargin{:});

  else
    ## for nargin 5 or 6, assign varargin terms to variables for 2-D algorithm
    xi = varargin{1};
    yi = varargin{2};

    ## Meshgrid if x and y are vectors but z is matrix
    if (isvector (x) && isvector (y) && all ([numel(y), numel(x)] == size (z)))
      [x, y] = meshgrid (x, y);
    endif

    if (isvector (x) && isvector (y) && isvector (z))
      if (! isequal (length (x), length (y), length (z)))
        error ("griddata: X, Y, and Z must be vectors of the same length");
      endif
    elseif (! size_equal (x, y, z))
      error ("griddata: lengths of X, Y must match the columns and rows of Z");
    endif

    ## Meshgrid xi and yi if they are a row and column vector, but not
    ## if they are simply vectors of the same size (for compatibility).
    if (isrow (xi) && iscolumn (yi))
      [xi, yi] = meshgrid (xi, yi);
    elseif (isvector (xi) && isvector (yi))
      ## Otherwise, convert to column vectors
      xi = xi(:);
      yi = yi(:);
    endif

    if (! size_equal (xi, yi))
      error ("griddata: XI and YI must be vectors or matrices of same size");
    endif

    if (nargin == 6)
      method = varargin{3};
      if (isempty (method))
        method = "linear";
      elseif (! ischar (method))
        error ("griddata: METHOD must be a string");
      else
        method = tolower (method);
      endif

      if (any (strcmp (method, {"linear", "nearest", "v4"})))
        ## Do nothing, these are implemented methods
      elseif (any (strcmp (method, {"cubic", "natural"})))
        ## FIXME: implement missing interpolation methods.
        error ('griddata: "%s" interpolation not yet implemented', method);
      else
        error ('griddata: unknown interpolation METHOD: "%s"', method);
      endif
    else
      method = "linear";
    endif

    x = x(:);
    y = y(:);
    z = z(:);

    ## Triangulate data.
    if (! strcmp (method, "v4"))
      tri = delaunay (x, y);
    endif
    zi = NaN (size (xi));

    if (strcmp (method, "linear"))
      ## Search for every point the enclosing triangle.
      tri_list = tsearch (x, y, tri, xi(:), yi(:));

      ## Only keep the points within triangles.
      valid = ! isnan (tri_list);
      tri_list = tri_list(valid);
      nr_t = rows (tri_list);

      tri = tri(tri_list,:);

      ## Assign x,y,z for each point of triangle.
      x1 = x(tri(:,1));
      x2 = x(tri(:,2));
      x3 = x(tri(:,3));

      y1 = y(tri(:,1));
      y2 = y(tri(:,2));
      y3 = y(tri(:,3));

      z1 = z(tri(:,1));
      z2 = z(tri(:,2));
      z3 = z(tri(:,3));

      ## Calculate norm vector.
      N = cross ([x2-x1, y2-y1, z2-z1], [x3-x1, y3-y1, z3-z1]);
      ## Normalize.
      N = diag (norm (N, "rows")) \ N;

      ## Calculate D of plane equation: Ax+By+Cz+D = 0
      D = -(N(:,1) .* x1 + N(:,2) .* y1 + N(:,3) .* z1);

      ## Calculate zi by solving plane equation for xi, yi.
      zi(valid) = -(N(:,1).*xi(:)(valid) + N(:,2).*yi(:)(valid) + D) ./ N(:,3);

    elseif (strcmp (method, "nearest"))
      ## Search index of nearest point.
      idx = dsearch (x, y, tri, xi, yi);
      valid = ! isnan (idx);
      zi(valid) = z(idx(valid));

    elseif (strcmp (method, "v4"))
      ## Use Biharmonic Spline Interpolation Green's Function method.
      ## Compatible with Matlab v4 interpolation method, based on
      ## D. Sandwell 1987 and Deng & Tang 2011.

      ## The free space Green Function which solves the two dimensional
      ## Biharmonic PDE
      ##
      ## Delta(Delta(G(X))) = delta(X)
      ##
      ## for a point source yields
      ##
      ## G(X) = |X|^2 * (ln|X|-1) / (8 * pi)
      ##
      ## An N-point Biharmonic Interpolation at the point X is given by
      ##
      ## z(X) = sum_j_N (alpha_j * G(X-Xj))
      ##      = sum_j_N (alpha_j * G(rj))
      ##
      ## in which the coefficients alpha_j are the unknowns.  rj is the
      ## Euclidean distance between X and Xj.
      ## From N datapoints {zi, Xi} an equation system can be formed:
      ##
      ## zi(Xi) = sum_j_N (alpha_j * G(Xi-Xj))
      ##        = sum_j_N (alpha_j * G(rij))
      ##
      ## Its inverse yields the unknowns alpha_j.

      ## Step1: Solve for weight coefficients alpha_j depending on the
      ## Euclidean distances and the training data set {x,y,z}
      r = sqrt ((x - x.').^2 + (y - y.').^2);  # size N^2
      D = (r.^2) .* (log (r) - 1);
      D(isnan (D)) = 0;  # Fix Green Function for r=0
      alpha_j = D \ z;

      ## Step2 - Use alphas and Green's functions to get interpolated points.
      ## Use dim3 projection for vectorized calculation to avoid loops.
      ## Memory usage is proportional to Ni x N.
      ## FIXME: if this approach is too memory intensive, revert portion to loop
      x = permute (x, [3, 2, 1]);
      y = permute (y, [3, 2, 1]);
      alpha_j = permute (alpha_j, [3, 2, 1]);
      r_i = sqrt ((xi - x).^2 + (yi - y).^2);  # size Ni x N
      Di = (r_i.^2) .* (log (r_i) - 1);
      Di(isnan (Di)) = 0;  # Fix Green's Function for r==0
      zi = sum (Di .* alpha_j, 3);

    endif

    if (nargout > 1)
      rx = xi;
      ry = yi;
      rz = zi;
    else
      rx = zi;
    endif

  endif

endfunction


%!demo
%! clf;
%! colormap ("default");
%! x = 2*rand (100,1) - 1;
%! y = 2*rand (size (x)) - 1;
%! z = sin (2*(x.^2 + y.^2));
%! [xx,yy] = meshgrid (linspace (-1, 1, 32));
%! zz = griddata (x,y,z,xx,yy);
%! mesh (xx, yy, zz);
%! title ("non-uniform grid sampled at 100 points");

%!demo
%! clf;
%! colormap ("default");
%! x = 2*rand (1000,1) - 1;
%! y = 2*rand (size (x)) - 1;
%! z = sin (2*(x.^2 + y.^2));
%! [xx,yy] = meshgrid (linspace (-1, 1, 32));
%! zz = griddata (x,y,z,xx,yy);
%! mesh (xx, yy, zz);
%! title ({"non-uniform grid sampled at 1,000 points",
%!         'method = "linear"'});

%!demo
%! clf;
%! colormap ("default");
%! x = 2*rand (1000,1) - 1;
%! y = 2*rand (size (x)) - 1;
%! z = sin (2*(x.^2 + y.^2));
%! [xx,yy] = meshgrid (linspace (-1, 1, 32));
%! zz = griddata (x,y,z,xx,yy,"nearest");
%! mesh (xx, yy, zz);
%! title ({"non-uniform grid sampled at 1,000 points",
%!         'method = "nearest neighbor"'});

%!testif HAVE_QHULL
%! [xx, yy] = meshgrid (linspace (-1, 1, 32));
%! x = xx(:);
%! x = x + 10*(2*round (rand (size (x))) - 1) * eps;
%! y = yy(:);
%! y = y + 10*(2*round (rand (size (y))) - 1) * eps;
%! z = sin (2*(x.^2 + y.^2));
%! zz = griddata (x,y,z,xx,yy, "linear");
%! zz2 = sin (2*(xx.^2 + yy.^2));
%! zz2(isnan (zz)) = NaN;
%! assert (zz, zz2, 100*eps);

%!testif HAVE_QHULL
%! [xx, yy] = meshgrid (linspace (-1, 1, 5));
%! x = xx(:);
%! x = x + 10*(2*round (rand (size (x))) - 1) * eps;
%! y = yy(:);
%! y = y + 10*(2*round (rand (size (y))) - 1) * eps;
%! z = 2*(x.^2 + y.^2);
%! zz = griddata (x,y,z,xx,yy, "v4");
%! zz2 = 2*(xx.^2 + yy.^2);
%! zz2(isnan (zz)) = NaN;
%! assert (zz, zz2, 100*eps);

%!testif HAVE_QHULL
%! [xx, yy] = meshgrid (linspace (-1, 1, 5));
%! x = xx(:);
%! x = x + 10*(2*round (rand (size (x))) - 1) * eps;
%! y = yy(:);
%! y = y + 10*(2*round (rand (size (y))) - 1) * eps;
%! z = 2*(x.^2 + y.^2);
%! zz = griddata (x,y,z,xx,yy, "nearest");
%! zz2 = 2*(xx.^2 + yy.^2);
%! zz2(isnan (zz)) = NaN;
%! assert (zz, zz2, 100*eps);

## Test input validation
%!error <Invalid call> griddata ()
%!error <Invalid call> griddata (1)
%!error <Invalid call> griddata (1,2)
%!error <Invalid call> griddata (1,2,3)
%!error <Invalid call> griddata (1,2,3,4)
%!error <only one output argument> [xi,yi] = griddata (1,2,3,4,5,6,7)
%!error <vectors of the same length> griddata (1:4, 1:3, 1:3, 1:3, 1:3)
%!error <vectors of the same length> griddata (1:3, 1:4, 1:3, 1:3, 1:3)
%!error <vectors of the same length> griddata (1:3, 1:3, 1:4, 1:3, 1:3)
%!error <the columns and rows of Z> griddata (1:4, 1:3, ones (4,4), 1:3, 1:3)
%!error <the columns and rows of Z> griddata (1:4, 1:3, ones (3,5), 1:3, 1:3)
%!error <XI and YI .* matrices of same size> griddata (1:3, 1:3, 1:3, 1:4, 1:3)
%!error <XI and YI .* matrices of same size> griddata (1:3, 1:3, 1:3, 1:3, 1:4)
%!error <METHOD must be a string> griddata (1,2,3,4,5, {"linear"})
%!error <"cubic" .* not yet implemented> griddata (1,2,3,4,5, "cubic")
%!error <"natural" .* not yet implemented> griddata (1,2,3,4,5, "natural")
%!error <unknown interpolation METHOD: "foobar"> griddata (1,2,3,4,5, "foobar")
