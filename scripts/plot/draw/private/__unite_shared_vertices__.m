## Copyright (C) 2016 Markus Muetzel
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
## @deftypefn {} {[@var{faces}, @var{vertices}, @var{J}] =} __unite_shared_vertices__ (@var{faces}, @var{vertices})
##
## Detect and unite shared vertices in patches
##
## Vertices of neighboring faces are detected and united to shared vertices. For
## this, the mutual squared distances between all vertices are calculated. If
## all coordinates are closer than @command{2 * eps (max (abs (vertices(:))))},
## the vertices are united to one.
##
## @var{J} holds the indices of the deleted vertices.
##
## @seealso{isosurface, reducepatch}
## @end deftypefn

## Author: mmuetzel

function [faces, vertices, J] = __unite_shared_vertices__ (faces, vertices)
  ### unite shared vertices

  J = [];
  ## Calculate the mutual differences of all vertex coordinates
  close_points = zeros (0, 2);
  num_vertices = size (vertices, 1);
  skip_point = false (num_vertices, 1);
  for (i_point1 = 1:num_vertices - 1)
    if (skip_point(i_point1))
      ## points already detected as duplicates can be skipped
      continue
    endif

    diff = vertices(i_point1,:) - vertices(i_point1 + 1:end,:);
    is_close_point = all (abs (diff) <= sqrt(3) * eps * ...
        (max (abs (vertices(i_point1,:)), abs (vertices(i_point1 + 1:end,:)))), 2);

    if (any (is_close_point))
      close_points_idx = find (is_close_point) + i_point1;
      new_close_points_num = size (close_points_idx, 1);
      close_points(end + 1:end + new_close_points_num,1) = i_point1;
      close_points(end - new_close_points_num + 1:end,2) = close_points_idx;
      skip_point(close_points_idx) = true;
    endif
  endfor

  if (! isempty (close_points))
    vertices(close_points(:,2),:) = []; # delete multiple shared vertices
    ## renumber deleted vertices in faces to the one it is replaced by
    vertex_renum = 1:num_vertices;
    vertex_renum(close_points(:,2)) = close_points(:,1);
    faces = vertex_renum(faces);
    ## renumber vertices in faces with subsequent numbers
    vertex_renum2 = ones (1, num_vertices);
    vertex_renum2(close_points(:,2)) = 0;
    vertex_renum2 = cumsum (vertex_renum2);
    faces = vertex_renum2(faces);

    ## eliminate identical faces
    faces = sort (faces, 2);
    faces = unique (faces, "rows");

    ## eliminate faces with zero area
    is_zero_area = (faces(:,1) == faces(:,2)) | (faces(:,2) == faces(:,3)); # vertices in faces are sorted
    faces = faces(!is_zero_area,:);

    J = close_points(:,2);
  endif

endfunction
