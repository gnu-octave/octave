########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{yi} =} griddatan (@var{x}, @var{y}, @var{xi})
## @deftypefnx {} {@var{yi} =} griddatan (@var{x}, @var{y}, @var{xi}, @var{method})
## @deftypefnx {} {@var{yi} =} griddatan (@var{x}, @var{y}, @var{xi}, @var{method}, @var{options})
##
## Interpolate irregular source data @var{x}, @var{y} at points specified by
## @var{xi}.
##
## The input @var{x} is an MxN matrix representing M points in an N-dimensional
## space.  The input @var{y} is a single-valued column vector (Mx1)
## representing a function evaluated at the points @var{x}, i.e.,
## @code{@var{y} = fcn (@var{x})}.  The input @var{xi} is a list of points
## for which the function output @var{yi} should be approximated through
## interpolation.  @var{xi} must have the same number of columns (@var{N})
## as @var{x} so that the dimensionality matches.
##
## The optional input interpolation @var{method} can be @qcode{"nearest"} or
## @qcode{"linear"}.  When the method is @qcode{"nearest"}, the output @var{yi}
## will be the closest point in the original data @var{x} to the query point
## @var{xi}.  When the method is @qcode{"linear"}, the output @var{yi} will
## be a linear interpolation between the two closest points in the original
## source data.  If @var{method} is omitted or empty, it defaults to
## @qcode{"linear"}.
##
## The optional argument @var{options} is passed directly to Qhull when
## computing the Delaunay triangulation used for interpolation.  See
## @code{delaunayn} for information on the defaults and how to pass different
## values.
##
## Example
##
## @example
## @group
## ## Evaluate sombrero() function at irregular data points
## x = 16*gallery ("uniformdata", [200,1], 1) - 8;
## y = 16*gallery ("uniformdata", [200,1], 11) - 8;
## z = sin (sqrt (x.^2 + y.^2)) ./ sqrt (x.^2 + y.^2);
## ## Create a regular grid and interpolate data
## [xi, yi] = ndgrid (linspace (-8, 8, 50));
## zi = griddatan ([x, y], z, [xi(:), yi(:)]);
## zi = reshape (zi, size (xi));
## ## Plot results
## clf ();
## plot3 (x, y, z, "or");
## hold on
## surf (xi, yi, zi);
## legend ("Original Data", "Interpolated Data");
## @end group
## @end example
##
## Programming Notes: If the input is complex the real and imaginary parts
## are interpolated separately.  Interpolation is based on a Delaunay
## triangulation and any query values outside the convex hull of the input
## points will return @code{NaN}.  For 2-D and 3-D data additional
## interpolation methods are available by using the @code{griddata} function.
## @seealso{griddata, griddata3, delaunayn}
## @end deftypefn

function yi = griddatan (x, y, xi, method = "linear", varargin)

  if (nargin < 3)
    print_usage ();
  endif

  [m, n] = size (x);
  [mi, ni] = size (xi);

  if (m < n + 1)
    error ("griddatan: number of points in X (rows of X) must be greater than dimensionality of data + 1 (columns of X + 1)");
  endif
  if (! iscolumn (y) || rows (y) != m)
    error ("griddatan: Y must be a column vector with the same number of points (rows) as X");
  endif
  if (n != ni)
    error ("griddatan: dimension of query data XI (columns) must match X");
  endif

  if (nargin > 3)
    if (isempty (method))
      method = "linear";
    elseif (! ischar (method))
      error ("griddatan: METHOD must be a string");
    else
      method = tolower (method);
    endif

    if (strcmp (method, "linear") || strcmp (method, "nearest"))
      ## Do nothing, these are implemented methods
    elseif (strcmp (method, "v4"))
      error ('griddatan: "%s" METHOD is available for 2-D inputs by using "griddata"', method);

    elseif (any (strcmp (method, {"cubic", "natural"})))
      ## FIXME: Remove when griddata.m supports these methods.
      error ('griddatan: "%s" interpolation METHOD not yet implemented', method);

    else
      error ('griddatan: unknown interpolation METHOD: "%s"', method);
    endif

  endif

  ## triangulate data
  tri = delaunayn (x, varargin{:});

  yi = NaN (mi, 1);

  if (strcmp (method, "linear"))
    ## search for every point the enclosing triangle
    [tri_list, bary_list] = tsearchn (x, tri, xi);

    ## only keep the points within triangles.
    valid = ! isnan (tri_list);
    tri_list = tri_list(valid);
    bary_list = bary_list(valid, :);
    nr_t = rows (tri_list);

    ## Use barycentric coordinate of point to calculate yi
    if (isscalar (tri_list))
      ## Special case required by orientation rules for vector/vector index.
      yi(valid) = sum (y(tri(tri_list,:)).' .* bary_list, 2);
    else
      yi(valid) = sum (y(tri(tri_list,:)) .* bary_list, 2);
    endif

  else
    ## search index of nearest point
    idx = dsearchn (x, tri, xi);
    valid = ! isnan (idx);
    yi(valid) = y(idx(valid));

  endif

endfunction


%!testif HAVE_QHULL
%! [xx,yy] = meshgrid (linspace (-1,1,32));
%! xi = [xx(:), yy(:)];
%! x = 2*rand (100,2) - 1;
%! x = [x;1,1;1,-1;-1,-1;-1,1];
%! y = sin (2 * sum (x.^2,2));
%! zz = griddatan (x,y,xi, "linear");
%! zz2 = griddata (x(:,1),x(:,2),y,xi(:,1),xi(:,2), "linear");
%! assert (zz, zz2, 1e-10);

%!testif HAVE_QHULL
%! [xx,yy] = meshgrid (linspace (-1,1,32));
%! xi = [xx(:), yy(:)];
%! x = 2*rand (100,2) - 1;
%! x = [x;1,1;1,-1;-1,-1;-1,1];
%! y = sin (2*sum (x.^2,2));
%! zz = griddatan (x,y,xi, "nearest");
%! zz2 = griddata (x(:,1),x(:,2),y,xi(:,1),xi(:,2), "nearest");
%! assert (zz, zz2, 1e-10);

%!testif HAVE_QHULL <*56515>
%! x = [ 0, 0; 1, 1; 0, 1; 1, 0 ];
%! y = [ 1; 2; 3; 4 ];
%! xi = [ .5, .5 ];
%! yi = griddatan (x, y, xi);

## Test input validation
%!error <Invalid call> griddatan ()
%!error <Invalid call> griddatan (1)
%!error <Invalid call> griddatan (1,2)
%!error <number of points in X> griddatan (1,2,3)
%!error <Y must be a column vector> griddatan ([1;2],[3,4], 1)
%!error <Y must .* same number of points .* as X> griddatan ([1;2],[3;4;5], 1)
%!error <dimension of .* XI .* must match X> griddatan ([1;2],[3;4], [1, 2])
%!error <METHOD must be a string> griddatan ([1;2],[3;4], 1, 5)
%!error <"v4" METHOD is available for 2-D> griddatan ([1;2],[3;4], 1, "v4")
%!error <"cubic" .* not yet implemented> griddatan ([1;2],[3;4], 1, "cubic")
%!error <"natural" .* not yet implemented> griddatan ([1;2],[3;4], 1, "natural")
%!error <unknown .* METHOD: "foobar"> griddatan ([1;2],[3;4], 1, "foobar")
