## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn  {Function File} {} trimesh (@var{tri}, @var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {@var{h} =} trimesh (@dots{})
## Plot a triangular mesh in 3D@.  The variable @var{tri} is the triangular
## meshing of the points @code{(@var{x}, @var{y})} which is returned
## from @code{delaunay}.  The variable @var{z} is value at the point
## @code{(@var{x}, @var{y})}.
##
## The optional return value @var{h} is a graphics handle to the created plot.
## @seealso{triplot, trisurf, delaunay3}
## @end deftypefn

function h = trimesh (tri, x, y, z, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  if (nargin == 3)
    triplot (tri, x, y);
  elseif (ischar (z))
    triplot (tri, x, y, z, varargin{:});
  else
    newplot ();
    handle = patch ("Vertices", [x(:), y(:), z(:)], "Faces", tri,
                    "FaceColor", "none", "EdgeColor", __next_line_color__(),
                    varargin{:});
    if (! ishold ())
      set (gca(), "view", [-37.5, 30],
           "xgrid", "on", "ygrid", "on", "zgrid", "on");
    endif
    if (nargout > 0)
      h = handle;
    endif
  endif

endfunction


%!demo
%! clf
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 10);
%! N = 10;
%! x = 3 - 6 * rand (N, N);
%! y = 3 - 6 * rand (N, N);
%! z = peaks (x, y);
%! tri = delaunay (x(:), y(:));
%! trimesh (tri, x(:), y(:), z(:));

