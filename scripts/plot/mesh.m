## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} mesh (@var{x}, @var{y}, @var{z})
## Plot a mesh given matrices @var{x}, and @var{y} from @code{meshgrid} and
## a matrix @var{z} corresponding to the @var{x} and @var{y} coordinates of
## the mesh.  If @var{x} and @var{y} are vectors, then a typical vertex
## is (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus, columns of @var{z}
## correspond to different @var{x} values and rows of @var{z} correspond
## to different @var{y} values.
## @seealso{meshgrid, contour}
## @end deftypefn

## Author: jwe

function h = mesh (x, y, z)

  newplot ();

  if (nargin == 1)
    z = x;
    if (ismatrix (z))
      [nr, nc] = size (z);
      x = 1:nc;
      y = (1:nr)';
    else
      error ("mesh: argument must be a matrix");
    endif
  elseif (nargin == 3)
    if (isvector (x) && isvector (y) && ismatrix (z))
      if (rows (z) == length (y) && columns (z) == length (x))
        x = x(:)';
        y = y(:);
      else
        msg = "mesh: rows (z) must be the same as length (y) and";
        msg = sprintf ("%s\ncolumns (z) must be the same as length (x)", msg);
        error (msg);
      endif
    elseif (ismatrix (x) && ismatrix (y) && ismatrix (z))
      if (! (size_equal (x, y) && size_equal (x, z)))
        error ("mesh: x, y, and z must have same dimensions");
      endif
    else
      error ("mesh: x and y must be vectors and z must be a matrix");
    endif
  else
    print_usage ();
  endif

  ## make a default line object, and make it the current axes for the
  ## current figure.
  ca = gca ();

  tmp = __go_surface__ (ca, "xdata", x, "ydata", y, "zdata", z);

  set (ca, "view", [-37.5, 30]);

  if (nargout > 0)
    h = tmp;
  endif

endfunction
