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
## @deftypefn  {} {@var{T} =} delaunayn (@var{pts})
## @deftypefnx {} {@var{T} =} delaunayn (@var{pts}, @var{options})
## Compute the Delaunay triangulation for an N-dimensional set of points.
##
## The Delaunay triangulation is a tessellation of the convex hull of a set of
## points such that no N-sphere defined by the N-triangles contains any other
## points from the set.
##
## The input matrix @var{pts} of size [n, dim] contains n points in a space of
## dimension dim.  The return matrix @var{T} has size [m, dim+1].  Each row of
## @var{T} contains a set of indices back into the original set of points
## @var{pts} which describes a simplex of dimension dim.  For example, a 2-D
## simplex is a triangle and 3-D simplex is a tetrahedron.
##
## An optional second argument, which must be a string or cell array of
## strings, contains options passed to the underlying qhull command.  See the
## documentation for the Qhull library for details
## @url{http://www.qhull.org/html/qh-quick.htm#options}.
## The default options depend on the dimension of the input:
##
## @itemize
## @item 2-D and 3-D: @var{options} = @code{@{"Qt", "Qbb", "Qc"@}}
##
## @item 4-D and higher: @var{options} = @code{@{"Qt", "Qbb", "Qc", "Qx"@}}
## @end itemize
##
## If Qhull fails for 2-D input the triangulation is attempted again with
## the options @code{@{"Qt", "Qbb", "Qc", "Qz"@}} which may result in
## reduced accuracy.
##
## If @var{options} is not present or @code{[]} then the default arguments are
## used.  Otherwise, @var{options} replaces the default argument list.
## To append user options to the defaults it is necessary to repeat the
## default arguments in @var{options}.  Use a null string to pass no arguments.
##
## @seealso{delaunay, convhulln, voronoin, trimesh, tetramesh}
## @end deftypefn

function T = delaunayn (pts, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  ## NOTE: varargin options input validation is performed in __delaunayn__
  if ((! isnumeric (pts)) || (ndims (pts) > 2))
    error ("delaunayn: input PTS must be a 2-dimensional numeric array");
  endif

  ## Perform delaunay calculation using either default or specified options
  if (isempty (varargin) || isempty (varargin{1}))
    try
      T = __delaunayn__ (pts);
    catch err
      if (columns (pts) <= 2)
        T = __delaunayn__ (pts, "Qt Qbb Qc Qz");
      else
        rethrow (err);
      endif
    end_try_catch
  else
    T = __delaunayn__ (pts, varargin{:});
  endif

  ## Begin check for and removal of trivial simplices
  if (! isequal (T, 0))  # skip trivial simplex check if no simplexes

    if (isa (pts, "single"))
      tol = 1e3 * eps ("single");
    else
      tol = 1e3 * eps;
    endif

    ## Try to remove the ~zero volume simplices.  The volume of the i-th simplex
    ## is given by abs(det(pts(T(i,2:end),:)-pts(T(i,1),:)))/factorial(ndim+1)
    ## (reference http://en.wikipedia.org/wiki/Simplex).  Any simplex with a
    ## relative volume less than some arbitrary criteria is rejected.  The
    ## criteria we use is the volume of a simplex corresponding to an
    ## orthogonal simplex (rectangle, rectangular prism, etc.) with edge lengths
    ## equal to the common-origin edge lengths of the original simplex.  If the
    ## relative volume is 1e3*eps then the simplex is rejected.  Note division
    ## of the two volumes means that the factor factorial(ndim+1) is dropped
    ## from volume calculations.

    [nt, nd] = size (T);  # nt = simplex count, nd = # of simplex points
    dim = nd - 1;

    ## Calculate common origin edge vectors for each simplex (p2-p1,p3-p1,...)
    ## Store in 3-D array such that:
    ## rows = nt simplexes, cols = coordinates, pages = simplex edges
    edge_vecs =  permute (reshape (pts(T(:, 2:nd), :).', [dim, nt, dim]), ...
                          [2, 1, 3]) - pts(T(:, 1), :, ones (1, 1, dim));

    ## Calculate orthogonal simplex volumes for comparison
    orthog_simplex_vols = sqrt (prod (sumsq (edge_vecs, 2), 3));

    ## Calculate simplex volumes according to problem dimension
    if (nd == 3)
      ## 2-D: area = cross product of triangle edge vectors
      vol = edge_vecs(:,1,1) .* edge_vecs(:,2,2) ...
            - edge_vecs(:,1,2) .* edge_vecs(:,2,1);

    elseif (nd == 4)
      ## 3-D: vol = scalar triple product [a.(b x c)]
      vol = edge_vecs(:,1,1) .* ...
              (edge_vecs(:,2,2) .* edge_vecs(:,3,3) - ...
                edge_vecs(:,3,2) .* edge_vecs(:,2,3)) ...
            - edge_vecs(:,2,1) .* ...
              (edge_vecs(:,1,2) .* edge_vecs(:,3,3) - ...
                edge_vecs(:,3,2) .* edge_vecs(:,1,3)) ...
            + edge_vecs(:,3,1) .* ...
              (edge_vecs(:,1,2) .* edge_vecs(:,2,3) - ...
                edge_vecs(:,2,2) .* edge_vecs(:,1,3));

    else
      ## 1-D and >= 4-D: simplex 'volume' proportional to det|edge_vecs|

      ## FIXME: Vectorize this for n-D inputs without excessive memory impact
      ## over __delaunayn__ itself, or move simplex checking into __delaunayn__;
      ## perhaps with an optimized page-wise determinant.
      ## See bug #60818 for speed/memory improvement attempts and concerns.
      vol = zeros (nt, 1);

      ## Reshape so det can operate in dim 1&2
      edge_vecs = permute (edge_vecs, [3, 2, 1]);

      ## Calculate determinant for arbitrary problem dimension
      for ii = 1:nt
        vol(ii) = det (edge_vecs(:, :, ii));
      endfor
    endif

    ## Mark simplices with relative volume < tol for removal
    idx = (abs ((vol) ./ orthog_simplex_vols)) < tol;

    ## Remove trivially small simplexes from T
    T(idx, :) = [];

    ## Ensure CCW node order for consistent outward normal (bug #53397)
    ## simplest method of maintaining positive unit normal direction is to
    ## reverse order of two nodes; this preserves 'nice' monotonic descending
    ## node 1 ordering.  Currently ignores 1-D cases for compatibility.
    if (dim > 1 && any (negvol = (vol(! idx) < 0)))
      T(negvol, [2, 3]) = T(negvol, [3, 2]);
    endif

  endif

endfunction


## Test 1-D input
%!testif HAVE_QHULL
%! assert (sortrows (sort (delaunayn ([1;2]), 2)), [1, 2]);
%! assert (sortrows (sort (delaunayn ([1;2;3]), 2)), [1, 2; 2, 3]);

## Test 2-D input
%!testif HAVE_QHULL
%! x = [-1, 0; 0, 1; 1, 0; 0, -1; 0, 0];
%! assert (sortrows (sort (delaunayn (x), 2)), [1,2,5;1,4,5;2,3,5;3,4,5]);

## Test 3-D input
%!testif HAVE_QHULL
%! x = [-1, -1, 1, 0, -1]; y = [-1, 1, 1, 0, -1]; z = [0, 0, 0, 1, 1];
%! assert (sortrows (sort (delaunayn ([x(:) y(:) z(:)]), 2)),
%!         [1,2,3,4;1,2,4,5]);

## 3-D test with trivial simplex removal
%!testif HAVE_QHULL
%! x = [0 0 0; 0 0 1; 0 1 0; 1 0 0; 0 1 1; 1 0 1; 1 1 0; 1 1 1; 0.5 0.5 0.5];
%! T = sortrows (sort (delaunayn (x), 2));
%! assert (rows (T), 12);

## 4-D single simplex test
%!testif HAVE_QHULL
%! x = [0 0 0 0; 1 0 0 0; 1 1 0 0; 0 0 1 0; 0 0 0 1];
%! T = sort (delaunayn (x), 2);
%! assert (T, [1 2 3 4 5]);

## 4-D two simplices test
%!testif HAVE_QHULL
%! x = [0 0 0 0; 1 0 0 0; 1 1 0 0; 0 0 1 0; 0 0 0 1; 0 0 0 2];
%! T = sortrows (sort (delaunayn (x), 2));
%! assert (rows (T), 2);
%! assert (T, [1 2 3 4 5; 2 3 4 5 6]);

## Test negative simplex produce positive normals
## 2-D test
%!testif HAVE_QHULL <*53397>
%! x = [-1, 0; 0, 1; 1, 0; 0, -1; 0, 0];
%! y = delaunayn (x);
%! edges = permute (reshape (x(y(:, 2:end), :).', [2, 4, 2]), [2, 1, 3]) - ...
%!         x(y(:, 1), :, ones (1, 1, 2));
%! vol = edges(:,1,1) .* edges(:,2,2) - edges(:,1,2) .* edges(:,2,1);
%! assert (all (vol >= 0));

## 3-D test
%!testif HAVE_QHULL <*53397>
%! x = [[-1, -1, 1, 0, -1]',[-1, 1, 1, 0, -1]',[0, 0, 0, 1, 1]'];
%! y = delaunayn (x);
%! edges = permute (reshape (x(y(:, 2:end), :).', [3, 2, 3]), [2, 1, 3]) - ...
%!         x(y(:, 1), :, ones (1, 1, 3));
%! vol = edges(:,1,1) .* ...
%!            (edges(:,2,2) .* edges(:,3,3) - edges(:,3,2) .* edges(:,2,3)) ...
%!       - edges(:,2,1) .* ...
%!            (edges(:,1,2) .* edges(:,3,3) - edges(:,3,2) .* edges(:,1,3)) ...
%!       + edges(:,3,1) .* ...
%!            (edges(:,1,2) .* edges(:,2,3) - edges(:,2,2) .* edges(:,1,3));
%! assert (all (vol >= 0));

## Input validation tests
%!error <Invalid call> delaunayn ()
%!error <input PTS must be> delaunayn ("abc")
%!error <input PTS must be> delaunayn ({1})
%!error <input PTS must be> delaunayn (true)
%!error <input PTS must be> delaunayn (ones (3,3,3))
