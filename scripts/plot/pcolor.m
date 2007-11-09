## Copyright (C) 2007 Kai Habel
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
## @deftypefn {Function File} {} pcolor (@var{x}, @var{y}, @var{c})
## @deftypefnx {Function File} {} pcolor (@var{c})
## Density plot for given matrices @var{x}, and @var{y} from @code{meshgrid} and
## a matrix @var{c} corresponding to the @var{x} and @var{y} coordinates of
## the mesh.  If @var{x} and @var{y} are vectors, then a typical vertex
## is (@var{x}(j), @var{y}(i), @var{c}(i,j)).  Thus, columns of @var{c}
## correspond to different @var{x} values and rows of @var{c} correspond
## to different @var{y} values.
## @seealso{meshgrid, contour}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function h = pcolor (x, y, c)

  newplot ();

  if (nargin == 1)
    c = x;
    z = zeros (size (c));
    [nr, nc] = size (c);
    [x, y] = meshgrid (1:nr, 1:nc);
  elseif (nargin == 3)
    z = zeros (size (c));
  else
    print_usage ();
  endif

  tmp = surface (x, y, z, c);

  ax = get (tmp, "parent");

  set (tmp, "facecolor", "flat");
  
  if (! ishold ())
    set (ax, "view", [0, 90]);
  endif

  if (nargout > 0)
    h = tmp;
  endif

endfunction
