## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2004,
##               2005, 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} @var{h} = {} surface (@var{x}, @var{y}, @var{z}, @var{c})
## @deftypefnx {Function File} @var{h} = {} surface (@var{x}, @var{y}, @var{z})
## Plot a surface graphic object given matrices @var{x}, and @var{y} from @code{meshgrid} and
## a matrix @var{z} corresponding to the @var{x} and @var{y} coordinates of
## the surface.  If @var{x} and @var{y} are vectors, then a typical vertex
## is (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus, columns of @var{z}
## correspond to different @var{x} values and rows of @var{z} correspond
## to different @var{y} values.
## @seealso{surf, mesh, patch, line}
## @end deftypefn

## Author: jwe

function h = surface (x, y, z, c)

  ax = gca ();

  if (nargin == 1)
    c = z = x;
    if (ismatrix (z))
      [nr, nc] = size (z);
      x = 1:nc;
      y = (1:nr)';
    else
      error ("surface: argument must be a matrix");
    endif
  elseif (nargin == 3)
    c = z;
    if (isvector (x) && isvector (y) && ismatrix (z))
      if (rows (z) == length (y) && columns (z) == length (x))
        x = x(:)';
        y = y(:);
      else
        error ("surface: rows (z) must be the same as length (y) and columns (z) must be the same as length (x)");
      endif
    elseif (ismatrix (x) && ismatrix (y) && ismatrix (z))
      if (! (size_equal (x, y) && size_equal (x, z)))
        error ("surface: x, y, and z must have same dimensions");
      endif
    else
      error ("surface: x and y must be vectors and z must be a matrix");
    endif
  elseif (nargin == 4)
    if (! size_equal (z, c))
      error ("surface: z and c must have same size");
    endif
    if (isvector (x) && isvector (y) && ismatrix (z))
      if (rows (z) == length (y) && columns (z) == length (x))
        x = x(:)';
        y = y(:);
      else
        error ("surface: rows (z) must be the same as length (y) and columns (z) must be the same as length (x)"
      endif
    elseif (ismatrix (x) && ismatrix (y) && ismatrix (z))
      if (! (size_equal (x, y) && size_equal (x, z)))
        error ("surface: x, y, and z must have same dimensions");
      endif
    else
      error ("surface: x and y must be vectors and z must be a matrix");
    endif
  else
    print_usage ();
  endif

  ## Make a default surface object.
  tmp = __go_surface__ (ax, "xdata", x, "ydata", y, "zdata", z, "cdata", c);

  set (ax, "view", [0, 90], "box", "off");
  set (tmp, "facecolor","flat");

  if (nargout > 0)
    h = tmp;
  endif

endfunction
