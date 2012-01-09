## Copyright (C) 2007-2012 Kai Habel
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
## @deftypefn  {Function File} {} pcolor (@var{x}, @var{y}, @var{c})
## @deftypefnx {Function File} {} pcolor (@var{c})
## Density plot for given matrices @var{x}, and @var{y} from @code{meshgrid} and
## a matrix @var{c} corresponding to the @var{x} and @var{y} coordinates of
## the mesh's vertices.  If @var{x} and @var{y} are vectors, then a typical
## vertex
## is (@var{x}(j), @var{y}(i), @var{c}(i,j)).  Thus, columns of @var{c}
## correspond to different @var{x} values and rows of @var{c} correspond
## to different @var{y} values.
##
## The @code{colormap} is scaled to the extents of @var{c}.
## Limits may be placed on the color axis by the
## command @code{caxis}, or by setting the @code{clim} property of the
## parent axis.
##
## The face color of each cell of the mesh is determined by interpolating
## the values of @var{c} for the cell's vertices.  Contrast this with
## @code{imagesc} which renders one cell for each element of @var{c}.
##
## @code{shading} modifies an attribute determining the manner by which the
## face color of each cell is interpolated from the values of @var{c},
## and the visibility of the cells' edges.  By default the attribute is
## "faceted", which renders a single color for each cell's face with the edge
## visible.
##
## @var{h} is the handle to the surface object.
##
## @seealso{caxis, contour, meshgrid, imagesc, shading}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function h = pcolor (x, y, c)

  newplot ();

  if (nargin == 1)
    c = x;
    [nr, nc] = size(c);
    z = zeros (nr, nc);
    [x, y] = meshgrid (1:nc, 1:nr);
  elseif (nargin == 3)
    z = zeros (size (c));
  else
    print_usage ();
  endif

  tmp = surface (x, y, z, c);

  ax = get (tmp, "parent");

  set (tmp, "facecolor", "flat");
  set (ax, "box", "on");

  if (! ishold ())
    set (ax, "view", [0, 90]);
  endif

  if (nargout > 0)
    h = tmp;
  endif

endfunction

%!demo
%! clf
%! [~,~,Z]=peaks;
%! pcolor(Z);

%!demo
%! clf
%! [X,Y,Z]=sombrero;
%! [Fx,Fy] = gradient(Z);
%! pcolor(X,Y,Fx+Fy);
%! shading interp;
