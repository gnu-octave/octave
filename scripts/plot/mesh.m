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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## usage: mesh (x, y, z)
##
## Surface plot.  If x, y, and z are matrices with the same dimensions,
## then corresponding elements represent vertices of the plot.  If x and
## y are vectors, then a typical vertex is (x(j), y(i), z(i,j)).  Thus,
## columns of z correspond to different x values and rows of z correspond
## to different y values.
##
## See also: plot, semilogx, semilogy, loglog, polar, meshgrid, meshdom,
##           contour, bar, stairs, gplot, gsplot, replot, xlabel, ylabel, title

## Author: jwe

function mesh (x, y, z)

  ## XXX FIXME XXX -- the plot states should really just be set
  ## temporarily, probably inside an unwind_protect block, but there is
  ## no way to determine their current values.

  if (nargin == 1)
    z = x;
    if (is_matrix (z))
      gset hidden3d;
      gset data style lines;
      gset surface;
      gset nocontour;
      gset noparametric;
      gset view 60, 30, 1, 1
      gsplot (z');
    else
      error ("mesh: argument must be a matrix");
    endif
  elseif (nargin == 3)
    if (is_vector (x) && is_vector (y) && is_matrix (z))
      xlen = length (x);
      ylen = length (y);
      if (xlen == columns (z) && ylen == rows (z))
        if (rows (y) == 1)
          y = y';
        endif
        len = 3 * xlen;
        zz = zeros (ylen, len);
        k = 1;
        for i = 1:3:len
          zz(:,i)   = x(k) * ones (ylen, 1);
          zz(:,i+1) = y;
          zz(:,i+2) = z(:,k);
          k++;
        endfor
	gset hidden3d;
	gset data style lines;
        gset surface;
        gset nocontour;
	gset parametric;
        gset view 60, 30, 1, 1
	gsplot (zz);
	gset noparametric;
      else
        msg = "mesh: rows (z) must be the same as length (x) and";
        msg = sprintf ("%s\ncolumns (z) must be the same as length (y)", msg);
        error (msg);
      endif
    elseif (is_matrix (x) && is_matrix (y) && is_matrix (z))
      xlen = columns (z);
      ylen = rows (z);
      if (xlen == columns (x) && xlen == columns (y) &&
	ylen == rows (x) && ylen == rows(y))
        len = 3 * xlen;
        zz = zeros (ylen, len);
        k = 1;
        for i = 1:3:len
          zz(:,i)   = x(:,k);
          zz(:,i+1) = y(:,k);
          zz(:,i+2) = z(:,k);
          k++;
        endfor
	gset hidden3d;
	gset data style lines;
        gset surface;
        gset nocontour;
	gset parametric;
        gset view 60, 30, 1, 1
	gsplot (zz);
	gset noparametric;
      else
        error ("mesh: x, y, and z must have same dimensions");
      endif
    else
      error ("mesh: x and y must be vectors and z must be a matrix");
    endif
  else
    usage ("mesh (z)");
  endif

endfunction
