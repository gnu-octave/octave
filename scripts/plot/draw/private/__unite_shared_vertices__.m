########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{faces}, @var{vertices}, @var{J}] =} __unite_shared_vertices__ (@var{faces}, @var{vertices})
##
## Detect and unite shared vertices in patches.
##
## Vertices of neighboring faces are detected and united to shared vertices.
## For this, the mutual squared distances between all vertices are
## calculated.  If all coordinates are closer than
## @code{2 * eps (max (abs (vertices(:))))}, the vertices are united to one.
##
## @var{J} holds the indices of the remaining vertices.
##
## @seealso{isosurface, reducepatch}
## @end deftypefn

function [faces, vertices, J] = __unite_shared_vertices__ (faces, vertices)

  nan_vertices = any (isnan (vertices), 2);
  lut = (1:rows (vertices))';
  J = lut;
  for di = 1:3
    [v, idx] = sortrows (vertices, 1 + mod ((0:2)+di, 3));
    Js = [true;
          any(abs (diff (v, 1, 1)) > eps * abs (v(2:end,:)+v(1:end-1,:)), 2)];
    vertices = v(Js,:);
    J = J(idx(Js));
    l(idx) = cumsum (Js);
    lut = l(lut);
  endfor
  [J, idx] = sort (J);
  j(idx) = 1:length (idx);
  vertices = vertices(idx,:);
  if (any (nan_vertices))
    j(end+1) = length (idx) + 1;
    vertices(end+1,:) = NaN;
    lut(nan_vertices) = rows (vertices);
  endif
  faces = j(lut(faces));

  ## Eliminate faces with zero area
  faces = faces(all (faces - faces(:, [2 3 1]) != 0, 2), :);

endfunction
