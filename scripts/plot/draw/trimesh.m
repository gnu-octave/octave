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
## @deftypefn  {} {} trimesh (@var{tri}, @var{x}, @var{y}, @var{z}, @var{c})
## @deftypefnx {} {} trimesh (@var{tri}, @var{x}, @var{y}, @var{z})
## @deftypefnx {} {} trimesh (@var{tri}, @var{x}, @var{y})
## @deftypefnx {} {} trimesh (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {@var{h} =} trimesh (@dots{})
## Plot a 3-D triangular wireframe mesh.
##
## In contrast to @code{mesh}, which plots a mesh using rectangles,
## @code{trimesh} plots the mesh using triangles.
##
## @var{tri} is typically the output of a Delaunay triangulation over the
## grid of @var{x}, @var{y}.  Every row of @var{tri} represents one triangle
## and contains three indices into [@var{x}, @var{y}] which are the
## vertices of the triangles in the x-y plane.  @var{z} determines the
## height above the plane of each vertex.  If no @var{z} input is given then
## the triangles are plotted as a 2-D figure.
##
## The color of the trimesh is computed by linearly scaling the @var{z} values
## to fit the range of the current colormap.  Use @code{caxis} and/or
## change the colormap to control the appearance.
##
## Optionally, the color of the mesh can be specified independently of @var{z}
## by supplying @var{c}, which is a vector for colormap data, or a matrix with
## three columns for RGB data.  The number of colors specified in @var{c} must
## either equal the number of vertices in @var{z} or the number of triangles
## in @var{tri}.
##
## Any property/value pairs are passed directly to the underlying patch object.
## The full list of properties is documented at @ref{Patch Properties}.
##
## The optional return value @var{h} is a graphics handle to the created patch
## object.
## @seealso{mesh, tetramesh, triplot, trisurf, delaunay, patch, hidden}
## @end deftypefn

function h = trimesh (tri, x, y, z, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  if (nargin == 3)
    htmp = triplot (tri, x, y);
  elseif (ischar (z))
    htmp = triplot (tri, x, y, z, varargin{:});
  else
    ## Process color argument
    if (nargin > 4 && isnumeric (varargin{1}))
      c = varargin{1};
      varargin(1) = [];
      if (isvector (c))
        c = c(:);
      endif
      if (rows (c) != numel (z) && rows (c) != rows (tri))
        error ("trimesh: the numbers of colors specified in C must equal the number of vertices in Z or the number of triangles in TRI");
      elseif (columns (c) != 1 && columns (c) != 3)
        error ("trimesh: TrueColor C matrix must have 3 columns");
      endif
    else
      c = z(:);
    endif

    hax = newplot ();

    ## Tag object as "trimesh" so that hidden() can find it.
    htmp = patch ("Faces", tri, "Vertices", [x(:), y(:), z(:)],
                  "FaceVertexCdata", c, "EdgeColor", "flat", "FaceColor", "w",
                  "FaceLighting", "none", "EdgeLighting", "flat",
                  "Tag", "trimesh", varargin{:});

    if (! ishold ())
      set (hax, "view", [-37.5, 30],
                "xgrid", "on", "ygrid", "on", "zgrid", "on");
    endif
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! colormap ("default");
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 10);
%! N = 10;
%! x = 3 - 6 * rand (N, N);
%! y = 3 - 6 * rand (N, N);
%! z = peaks (x, y);
%! tri = delaunay (x(:), y(:));
%! trimesh (tri, x(:), y(:), z(:));
%! title ("trimesh() plot of sparsely-sampled peaks() function");

## Test input validation
%!error <Invalid call> trimesh ()
%!error <Invalid call> trimesh (1)
%!error <Invalid call> trimesh (1,2)
%!error <the numbers of colors> trimesh (1,2,3,4,[5 6])
%!error <the numbers of colors> trimesh (1,2,3,4,[5 6]')
%!error <the numbers of colors> trimesh ([1;1],[2;2],[3;3],[4;4], zeros (3,3))
%!error <TrueColor C matrix must have 3 columns>
%! trimesh ([1;1],[2;2],[3;3],[4;4],zeros (2,2))
