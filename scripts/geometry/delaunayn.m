## Copyright (C) 2007, 2008, 2009 David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{T} =} delaunayn (@var{P})
## @deftypefnx {Function File} {@var{T} =} delaunayn (@var{P}, @var{opt})
## Form the Delaunay triangulation for a set of points.
## The Delaunay triangulation is a tessellation of the convex hull of the
## points such that no n-sphere defined by the n-triangles contains
## any other points from the set.
## The input matrix @var{P} of size @code{[n, dim]} contains @var{n}
## points in a space of dimension dim.  The return matrix @var{T} has the
## size @code{[m, dim+1]}.  It contains for each row a set of indices to
## the points, which describes a simplex of dimension dim.  For example,
## a 2d simplex is a triangle and 3d simplex is a tetrahedron.
## 
## Extra options for the underlying Qhull command can be specified by the
## second argument.  This argument is a cell array of strings.  The default
## options depend on the dimension of the input: 
## 
## @itemize 
## @item 2D and 3D: @var{opt} = @code{@{"Qt", "Qbb", "Qc"@}}
##
## @item 4D and higher: @var{opt} = @code{@{"Qt", "Qbb", "Qc", "Qz"@}} 
## @end itemize
## 
## If @var{opt} is [], then the default arguments are used.  If @var{opt}
## is @code{@{"@w{}"@}}, then none of the default arguments are used by Qhull. 
## See the Qhull documentation for the available options. 
## 
## All options can also be specified as single string, for example
## @code{"Qt Qbb Qc Qz"}.
## 
## @end deftypefn

function t = delaunayn (x, varargin)
  if (nargin < 1)
    print_usage ();
  endif

  t = __delaunayn__ (x, varargin{:});

  if (isa (x, "single"))
    myeps = eps ("single");
  else
    myeps = eps;
  endif

  ## Try to remove the zero volume simplices. The volume of the i-th simplex is
  ## given by abs(det(x(t(i,1:end-1),:)-x(t(i,2:end),:)))/prod(1:n) 
  ## (reference http://en.wikipedia.org/wiki/Simplex). Any simplex with a 
  ## relative volume less than some arbitrary criteria is rejected. The 
  ## criteria we use is the volume of the simplex corresponding to an 
  ## orthogonal simplex is equal edge length all equal to the edge length of 
  ## the original simplex. If the relative volume is 1e3*eps then the simplex
  ## is rejected. Note division of the two volumes means that the factor 
  ## prod(1:n) is dropped.
  idx = [];
  [nt, n] = size (t);
  for i = 1:nt
    X = x(t(i,1:end-1),:) - x(t(i,2:end),:);
    if (abs (det (X)) /  sqrt (sum (X .^ 2, 2)) < 1e3 * myeps)
     idx = [idx, i];
    endif
  endfor
  t(idx,:) = [];
endfunction
