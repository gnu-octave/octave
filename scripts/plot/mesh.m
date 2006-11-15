## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

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

function mesh (x, y, z)

  __plot_globals__;

  cf = __current_figure__;
  mxi = __multiplot_xi__(cf);
  myi = __multiplot_yi__(cf);

  if (nargin == 1)
    z = x;
    if (ismatrix (z))
      [x, y] = meshgrid(0:columns(z)-1, 0:rows(z)-1);
    else
      error ("mesh: argument must be a matrix");
    endif
  elseif (nargin == 3)
    if (isvector (x) && isvector (y) && ismatrix (z))
      if (rows (z) == length (y) && columns (z) == length (x))
        x = repmat(x(:)', rows (z), 1);
        y = repmat(y(:), 1, columns (z));
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

  ## Show the mesh.
  xlen = columns (z);
  ylen = rows (z);
  if (xlen == columns (x) && xlen == columns (y)
      && ylen == rows (x) && ylen == rows (y))
    len = 3 * xlen;
    zz = zeros (ylen, len);
    k = 1;
    for i = 1:3:len
      zz(:,i)   = x(:,k);
      zz(:,i+1) = y(:,k);
      zz(:,i+2) = z(:,k);
      k++;
    endfor
    __gnuplot_raw__ ("set hidden3d;\n");
    __gnuplot_raw__ ("set data style lines;\n");
    __gnuplot_raw__ ("set surface;\n");
    __gnuplot_raw__ ("set nocontour;\n");
    __gnuplot_raw__ ("set nologscale;\n");
    __gnuplot_raw__ ("set view 60, 30, 1, 1;\n");
    __gnuplot_raw__ ("set palette defined (0 \"dark-blue\", 1 \"blue\", 2 \"cyan\", 3 \"yellow\", 4 \"red\" , 5 \"dark-red\");\n");
    __gnuplot_raw__ ("set nocolorbox;\n");
    __plt3__ (zz, true, "", "", "",
	      sprintf ("%s line palette", gnuplot_command_with ()));
  else
    error ("mesh: x, y, and z must have same dimensions");
  endif

endfunction
