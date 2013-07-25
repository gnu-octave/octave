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
## @deftypefn {Function File} {} meshz (@var{x}, @var{y}, @var{z})
## Plot a curtain mesh given matrices @var{x}, and @var{y} from
## @code{meshgrid} and a matrix @var{z} corresponding to the @var{x} and
## @var{y} coordinates of the mesh.  If @var{x} and @var{y} are vectors,
## then a typical vertex is (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus,
## columns of @var{z} correspond to different @var{x} values and rows of
## @var{z} correspond to different @var{y} values.
## @seealso{meshgrid, mesh, contour}
## @end deftypefn

function h = meshz (varargin)

  if (! all (cellfun ("isreal", varargin)))
    error ("meshz: X, Y, Z, C arguments must be real");
  endif

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("meshz", varargin{:});

  ## Find where property/value pairs start
  charidx = find (cellfun ("isclass", varargin, "char"), 1);

  if (isempty (charidx))
    if (nargin == 2 || nargin == 4) 
      charidx = nargin;   # bundle C matrix back into varargin 
    else
      charidx = nargin + 1;
    endif
  endif

  if (charidx == 2)
    z = varargin{1};
    [m, n] = size (z);
    x = 1:n;
    y = (1:m).';
  else
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
  endif

  if (isvector (x) && isvector (y))
    x = [x(1), x(:).', x(end)];
    y = [y(1); y(:); y(end)];
  else
    x = [x(1,1), x(1,:), x(1,end);
         x(:,1), x, x(:,end);
         x(end,1), x(end,:), x(end,end)];
    y = [y(1,1), y(1,:), y(1,end);
         y(:,1), y, y(:,end);
         y(end,1), y(end,:), y(end,end)];
  endif

  zref = min (z(isfinite (z)));
  z = [zref .* ones(1, columns(z) + 2);
       zref .* ones(rows(z), 1), z, zref .* ones(rows(z), 1);
       zref .* ones(1, columns(z) + 2)];

  oldfig = ifelse (isempty (hax), [], get (0, "currentfigure"));
  unwind_protect
    hax = newplot (hax);
    htmp = mesh (hax, x, y, z, varargin{charidx:end});
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction

