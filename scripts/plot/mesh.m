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

## -*- texinfo -*-
## @deftypefn {Function File} {} mesh (@var{x}, @var{y}, @var{z})
## Plot a mesh given matrices @var{x}, and @var{y} from @code{meshdom} and
## a matrix @var{z} corresponding to the @var{x} and @var{y} coordinates of
## the mesh.  If @var{x} and @var{y} are vectors, then a typical vertex
## is (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus, columns of @var{z}
## correspond to different @var{x} values and rows of @var{z} correspond
## to different @var{y} values.
## @end deftypefn
##
## @seealso{plot, semilogx, semilogy, loglog, polar, meshgrid, meshdom,
## contour, bar, stairs, replot, xlabel, ylabel, and title}

## Author: jwe

function mesh (x, y, z)

  ## XXX FIXME XXX -- the plot states should really just be set
  ## temporarily, probably inside an unwind_protect block, but there is
  ## no way to determine their current values.

  if (nargin == 1)
    z = x;
    if (ismatrix (z))
      __gset__ hidden3d;
      __gset__ data style lines;
      __gset__ surface;
      __gset__ nocontour;
      __gset__ noparametric;
      __gset__ nologscale;
      __gset__ view 60, 30, 1, 1
      __gsplot__ (z');
    else
      error ("mesh: argument must be a matrix");
    endif
  elseif (nargin == 3)
    if (isvector (x) && isvector (y) && ismatrix (z))
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
        __gset__ hidden3d;
        __gset__ data style lines;
        __gset__ surface;
        __gset__ nocontour;
        __gset__ nologscale;
        __gset__ parametric;
        __gset__ view 60, 30, 1, 1
        __gsplot__ (zz);
        __gset__ noparametric;
      else
        msg = "mesh: rows (z) must be the same as length (y) and";
        msg = sprintf ("%s\ncolumns (z) must be the same as length (x)", msg);
        error (msg);
      endif
    elseif (ismatrix (x) && ismatrix (y) && ismatrix (z))
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
        __gset__ hidden3d;
        __gset__ data style lines;
        __gset__ surface;
        __gset__ nocontour;
        __gset__ nologscale;
        __gset__ parametric;
        __gset__ view 60, 30, 1, 1
        __gsplot__ (zz);
        __gset__ noparametric;
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
