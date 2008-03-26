## Copyright (C) 2007 David Bateman
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
## @deftypefn {Function File} {} trimesh (@var{tri}, @var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {@var{h} =} trimesh (@dots{})
## Plot a triangular mesh in 3D. The variable @var{tri} is the triangular
## meshing of the points @code{(@var{x}, @var{y})} which is returned 
## from @code{delaunay}. The variable @var{z} is value at the point 
## @code{(@var{x}, @var{y})}. The output argument @var{h} is the graphic 
## handle to the plot.
## @seealso{triplot, delaunay3}
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
    idx = tri(:, [1,2,3,1]).';
    nt = size (tri, 1);
    ## FIXME We should really use patch instead of plot3, but we don't
    ## have a patch function, and probably won't in 3D that works with
    ## gnuplot
    if (nargout > 0)
      h = plot3 ([x(idx); NaN*ones(1, nt)](:),
		 [y(idx); NaN*ones(1, nt)](:),
		 [z(idx); NaN*ones(1, nt)](:), varargin{:});
    else
      plot3 ([x(idx); NaN*ones(1, nt)](:),
	     [y(idx); NaN*ones(1, nt)](:),
	     [z(idx); NaN*ones(1, nt)](:), varargin{:});
    endif
  endif
endfunction

%!demo
%! rand ('state', 10)
%! x = 3 - 6 * rand (1, 50);
%! y = 3 - 6 * rand (1, 50);
%! z = peaks (x, y);
%! tri = delaunay (x, y);
%! trimesh (tri, x, y, z);
