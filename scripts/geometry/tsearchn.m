########################################################################
##
## Copyright (C) 2007-2022 The Octave Project Developers
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
## @deftypefn  {} {@var{idx} =} tsearchn (@var{x}, @var{t}, @var{xi})
## @deftypefnx {} {[@var{idx}, @var{p}] =} tsearchn (@var{x}, @var{t}, @var{xi})
## Find the simplexes enclosing the given points.
##
## @code{tsearchn} is typically used with @code{delaunayn}:
## @code{@var{t} = delaunayn (@var{x})} returns a set of simplexes @code{t},
## then @code{tsearchn} returns the row index of @var{t} containing each point
## of @var{xi}.  For points outside the convex hull, @var{idx} is NaN.
##
## If requested, @code{tsearchn} also returns the barycentric coordinates
## @var{p} of the enclosing simplexes.
##
## @seealso{delaunay, delaunayn, tsearch}
## @end deftypefn

function [idx, p] = tsearchn (x, t, xi)

  if (nargin != 3)
    print_usage ();
  endif

  if (columns (x) != columns (xi))
    error ("columns (x) should equal columns (xi)")
  end

  if (max (t(:)) > rows (x))
    error ("triangles should only access points in x")
  end

  if (nargout <= 1 && columns (x) == 2)  # pass to the faster tsearch.cc
    idx = tsearch (x(:,1), x(:,2), t, xi(:,1), xi(:,2));
    return
  endif

  nt = rows (t);
  [m, n] = size (x);
  mi = rows (xi);
  idx = NaN (mi, 1);
  p = NaN (mi, n + 1);
  ni = [1:mi].';

  for i = 1 : nt  # each simplex in turn

    T = x(t(i, :), :);  # T is the current simplex
    P = xi(ni, :);      # P is the set of points left to calculate

    ## Convert to barycentric coords: these are used to express a point P
    ## as    P = Beta * T
    ## where T is a simplex.
    ##
    ## If 0 <= Beta <= 1, then the linear combination is also convex,
    ## and the point P is inside the simplex T, otherwise it is outside.
    ## Since the equation system is underdetermined, we apply the constraint
    ## sum (Beta) == 1  to make it unique up to scaling.
    ##
    ## Note that the code below is vectorized over P, one point per row.

    b = (P - T(end,:)) / (T(1:end-1,:) - T(end,:));
    b(:, end+1) = 1 - sum (b, 2);

    ## The points xi are inside the current simplex if
    ## (all (b >= 0) && all (b <= 1)).  As sum (b,2) == 1, we only need to
    ## test all(b>=0).
    inside = all (b >= -1e-12, 2);  # -1e-12 instead of 0 for rounding errors
    idx (ni (inside)) = i;
    p(ni(inside), :) = b(inside, :);
    ni = ni (~inside);
  endfor

endfunction

%!shared x, tri
%! x = [-1,-1;-1,1;1,-1];
%! tri = [1, 2, 3];
%!test
%! [idx, p] = tsearchn (x,tri,[-1,-1]);
%! assert (idx, 1);
%! assert (p, [1,0,0], 1e-12);
%!test
%! [idx, p] = tsearchn (x,tri,[-1,1]);
%! assert (idx, 1);
%! assert (p, [0,1,0], 1e-12);
%!test
%! [idx, p] = tsearchn (x,tri,[1,-1]);
%! assert (idx, 1);
%! assert (p, [0,0,1], 1e-12);
%!test
%! [idx, p] = tsearchn (x,tri,[-1/3,-1/3]);
%! assert (idx, 1);
%! assert (p, [1/3,1/3,1/3], 1e-12);
%!test
%! [idx, p] = tsearchn (x,tri,[1,1]);
%! assert (idx, NaN);
%! assert (p, [NaN, NaN, NaN]);
