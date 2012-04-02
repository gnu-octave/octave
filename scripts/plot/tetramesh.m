## Copyright (C) 2012 Martin Helm
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
## @deftypefn  {Function File} tetramesh (@var{T}, @var{X})
## @deftypefnx {Function File} tetramesh (@var{T}, @var{X}, @var{C})
## @deftypefnx {Function File} {[@var{h}] =} tetramesh (...)
## @deftypefnx {Function File} {[@var{h}] =} tetramesh (..., @var{PROP}, @var{VAL})
##
## The function displays the tetrahedrons defined in the m by 4 matrix @var{T}
## as 3D patches. @var{T} is usually the output of a Delaunay triangulation of a
## 3D set of points. 
## Every row of @var{T} contains four indices into the n by 3 matrix @var{X} 
## of the vertices of a tetrahedron. 
## Every row in @var{X} represents one point in 3D space. 
##
## If the vector @var{C} is supplied it must contain indices into the current 
## colormap. Called without @var{C} it is set to 1:m, where m is the number of
## tetrahedrons, the indices are scaled to map to the full range of the colormap. 
## If more tetrahedrons than entries in the colormap are given the entries of
## @var{C} are cyclic repeated.
## 
## When called with one output argument @var{H} it returns a vector of patch 
##  handles,each representing one tetrahedron in the order given by @var{T}. 
## One use case for @var{H} is to turn the respective patch 'Visible' property 
## 'on' or 'off'.
##
## Calling tetramesh(...,'param','value','param','value'...) passes all
## option/value pairs directly as additional arguments to the patch function for
## every tetrahedron.
##
## The command
##
##@example
## @group
## demo tetramesh
## @end group
## @end example
##
## @noindent
## will show some examples how to use it.
#### @seealso{patch}
## @end deftypefn

## Author: Martin Helm <martin@mhelm.de>

function [h] = tetramesh (varargin)

  [reg, prop] = parseparams (varargin);

  if (length (reg) < 2 || length (reg) > 3)
    print_usage ()
  endif

  T = reg{1};
  X = reg{2};

  if (! ismatrix (T) || size (T, 2) != 4)
    error ("tetramesh: T must be a n by 4 matrix")
  endif
  if (! ismatrix (X) || size (X, 2) != 3)
    error ("tetramesh: X must be a n by 3 matrix")
  endif

  size_T = size (T, 1);
  colmap = colormap ();
  
  # do we need to enable gnuplot workaround?
  shrink = strcmp (graphics_toolkit (), "gnuplot");

  if (length (reg) < 3)
    size_colmap = size (colmap, 1);
    C = mod ((1:size_T)' - 1, size_colmap) + 1;
    if (size_T < size_colmap && size_T > 1) 
      # expand to the available range of colors
      C = floor ((C - 1) * (size_colmap - 1) / (size_T - 1)) + 1;
    endif
  else
    C = reg{3};
    if (! isvector (C) || size_T != length (C))
      error ("tetramesh: C must be a vector of the same length as T")
    endif
  endif

  h = zeros (1, size_T);
  if (shrink)
    # tiny reduction of the tetrahedron size to help gnuplot by
    # avoiding identical faces with different colors
    for ii = 1:size_T
      [th, p] = __shrink__ ([1 2 3 4], X(T(ii, :), :), 1 - 1e-7);
      h(ii) = patch ("Faces", th, "Vertices", p, "FaceColor", ...
                     colmap(C(ii), :), prop{:});
    endfor
  else
    for ii = 1:size_T
      th = [1 2 3; 2 3 4; 3 4 1; 4 1 2];
      h(ii) = patch ("Faces", th, "Vertices", X(T(ii, :), :), "FaceColor", ...
                     colmap(C(ii), :), prop{:});
    endfor
  endif

  if (nargout == 0) #return nothing
    clear h;
  endif
endfunction

## shrink the tetrahedron relative to its center of gravity
function [tri, p] = __shrink__ (T, X, sf)
  midpoint = repmat (sum (X(T, :), 1) / 4, 12, 1);
  p = [X([1 2 3], :); X([2 3 4], :); X([3 4 1], :); X([4 1 2], :)];
  p = sf * (p - midpoint) + midpoint;
  tri = reshape (1:12, 3, 4)';
endfunction

%!demo
%! d = [-1 1];
%! [x,y,z] = meshgrid (d, d, d);
%! x = [x(:); 0];
%! y = [y(:); 0];
%! z = [z(:); 0];
%! tetra = delaunay3 (x, y, z);
%! X = [x(:) y(:) z(:)];
%! clf ()
%! colormap (jet (64))
%! h = tetramesh (tetra, X);
%! for ii=1:2:length(h);
%!   set(h(ii), "Visible", "off");
%! endfor
%! axis equal
%! view (30, 20)
%! title ("Using jet (64), every other tetrahedron invisible")

%!demo
%! d = [-1 1];
%! [x,y,z] = meshgrid (d, d, d);
%! x = [x(:); 0];
%! y = [y(:); 0];
%! z = [z(:); 0];
%! tetra = delaunay3 (x, y, z);
%! X = [x(:) y(:) z(:)];
%! clf ()
%! colormap (gray (256));
%! tetramesh (tetra, X, 21:20:241, "EdgeColor", "w")
%! axis equal
%! view (30, 20)
%! title ("Using gray (256) and white edges")
