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
## @deftypefn  {Function File} {} surfnorm (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} surfnorm (@var{z})
## @deftypefnx {Function File} {[@var{nx}, @var{ny}, @var{nz}] =} surfnorm (@dots{})
## @deftypefnx {Function File} {} surfnorm (@var{h}, @dots{})
## Find the vectors normal to a meshgridded surface.  The meshed gridded
## surface is defined by @var{x}, @var{y}, and @var{z}.  If @var{x} and
## @var{y} are not defined, then it is assumed that they are given by
##
## @example
## @group
## [@var{x}, @var{y}] = meshgrid (1:size (@var{z}, 1),
##                    1:size (@var{z}, 2));
## @end group
## @end example
##
## If no return arguments are requested, a surface plot with the normal
## vectors to the surface is plotted.  Otherwise the components of the normal
## vectors at the mesh gridded points are returned in @var{nx}, @var{ny},
## and @var{nz}.
##
## The normal vectors are calculated by taking the cross product of the
## diagonals of each of the quadrilaterals in the meshgrid to find the
## normal vectors of the centers of these quadrilaterals.  The four nearest
## normal vectors to the meshgrid points are then averaged to obtain the
## normal to the surface at the meshgridded points.
##
## An example of the use of @code{surfnorm} is
##
## @example
## surfnorm (peaks (25));
## @end example
## @seealso{surf, quiver3}
## @end deftypefn

function [Nx, Ny, Nz] = surfnorm (varargin)

  [h, varargin, nargin] = __plt_get_axis_arg__ ((nargout != 0), "surfnorm",
                                                varargin{:});

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (nargin == 1)
    z = varargin{1};
    [x, y] = meshgrid (1:size(z,1), 1:size(z,2));
    ioff = 2;
  else
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
    ioff = 4;
  endif

  if (!ismatrix (z) || isvector (z) || isscalar (z))
    error ("surfnorm: Z argument must be a matrix");
  endif
  if (! size_equal (x, y, z))
    error ("surfnorm: X, Y, and Z must have the same dimensions");
  endif

  ## Make life easier, and avoid having to do the extrapolation later, do
  ## a simpler linear extrapolation here. This is approximative, and works
  ## badly for closed surfaces like spheres.
  xx = [2 .* x(:,1) - x(:,2), x, 2 .* x(:,end) - x(:,end-1)];
  xx = [2 .* xx(1,:) - xx(2,:); xx; 2 .* xx(end,:) - xx(end-1,:)];
  yy = [2 .* y(:,1) - y(:,2), y, 2 .* y(:,end) - y(:,end-1)];
  yy = [2 .* yy(1,:) - yy(2,:); yy; 2 .* yy(end,:) - yy(end-1,:)];
  zz = [2 .* z(:,1) - z(:,2), z, 2 .* z(:,end) - z(:,end-1)];
  zz = [2 .* zz(1,:) - zz(2,:); zz; 2 .* zz(end,:) - zz(end-1,:)];

  u.x = xx(1:end-1,1:end-1) - xx(2:end,2:end);
  u.y = yy(1:end-1,1:end-1) - yy(2:end,2:end);
  u.z = zz(1:end-1,1:end-1) - zz(2:end,2:end);
  v.x = xx(1:end-1,2:end) - xx(2:end,1:end-1);
  v.y = yy(1:end-1,2:end) - yy(2:end,1:end-1);
  v.z = zz(1:end-1,2:end) - zz(2:end,1:end-1);

  c = cross ([u.x(:), u.y(:), u.z(:)], [v.x(:), v.y(:), v.z(:)]);
  w.x = reshape (c(:,1), size(u.x));
  w.y = reshape (c(:,2), size(u.y));
  w.z = reshape (c(:,3), size(u.z));

  ## Create normal vectors as mesh vectices from normals at mesh centers
  nx = (w.x(1:end-1,1:end-1) + w.x(1:end-1,2:end) +
        w.x(2:end,1:end-1) + w.x(2:end,2:end)) ./ 4;
  ny = (w.y(1:end-1,1:end-1) + w.y(1:end-1,2:end) +
        w.y(2:end,1:end-1) + w.y(2:end,2:end)) ./ 4;
  nz = (w.z(1:end-1,1:end-1) + w.z(1:end-1,2:end) +
        w.z(2:end,1:end-1) + w.z(2:end,2:end)) ./ 4;

  ## Normalize the normal vectors
  len = sqrt (nx.^2 + ny.^2 + nz.^2);
  nx = nx ./ len;
  ny = ny ./ len;
  nz = nz ./ len;

  if (nargout == 0)
    oldh = gca ();
    unwind_protect
      axes (h);
      newplot ();
      surf (x, y, z, varargin{ioff:end});
      old_hold_state = get (h, "nextplot");
      unwind_protect
        set (h, "nextplot", "add");
        plot3 ([x(:)'; x(:).' + nx(:).' ; NaN(size(x(:).'))](:),
               [y(:)'; y(:).' + ny(:).' ; NaN(size(y(:).'))](:),
               [z(:)'; z(:).' + nz(:).' ; NaN(size(z(:).'))](:),
               varargin{ioff:end});
      unwind_protect_cleanup
        set (h, "nextplot", old_hold_state);
      end_unwind_protect
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  else
    Nx = nx;
    Ny = ny;
    Nz = nz;
  endif

endfunction

%!demo
%! clf
%! colormap (jet (64))
%! [x, y, z] = peaks(10);
%! surfnorm (x, y, z);

%!demo
%! clf
%! surfnorm (peaks(10));

%!demo
%! clf
%! surfnorm (peaks(32));
%! shading interp
