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
## @deftypefn {Function File} {} contour (@var{z}, @var{n}, @var{x}, @var{y})
## Make a contour plot of the three-dimensional surface described by
## @var{z}.  Someone needs to improve @code{gnuplot}'s contour routines
## before this will be very useful.
## @end deftypefn
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour,
## bar, stairs, gplot, gsplot, replot, xlabel, ylabel, and title}

## Author: jwe

function contour (z, n, x, y)

  if (nargin == 1)
    n = 10;
  endif

  ## XXX FIXME XXX -- these plot states should really just be set
  ## temporarily, probably inside an unwind_protect block, but there is
  ## no way to determine their current values.

  if (nargin == 1 || nargin == 2)
    if (is_matrix (z))
      gset nosurface;
      gset contour;
      gset cntrparam bspline;
      if (is_scalar (n))
	command = sprintf ("gset cntrparam levels %d", n);
      elseif (is_vector (n))
	tmp = sprintf ("%f", n(1));
	for i = 2:length (n)
	  tmp = sprintf ("%s, %f", tmp, n(i));
	endfor
	command = sprintf ("gset cntrparam levels discrete %s", tmp);
      endif
      eval (command);
      gset noparametric;
      gset view 0, 0, 1, 1;
      gsplot z w l 1;
    else
      error ("contour: argument must be a matrix");
    endif
  elseif (nargin == 4)
    if (is_vector (x) && is_vector (y) && is_matrix (z))
      xlen = length (x);
      ylen = length (y);
      if (xlen == rows (z) && ylen == columns (z))
        if (rows (x) == 1)
          x = x';
        endif
        len = 3 * ylen;
        zz = zeros (xlen, len);
        k = 1;
        for i = 1:3:len
          zz(:,i)   = x;
          zz(:,i+1) = y(k) * ones (xlen, 1);
          zz(:,i+2) = z(:,k);
          k++;
        endfor
        gset nosurface;
        gset contour;
        gset cntrparam bspline;
	if (is_scalar (n))
          command = sprintf ("gset cntrparam levels %d", n);
	elseif (is_vector (n))
	  tmp = sprintf ("%f", n(1));
	  for i = 2:length (n)
	    tmp = sprintf ("%s, %f", tmp, n(i));
	  endfor
	  command = sprintf ("gset cntrparam levels discrete %s", tmp);
	endif
        eval (command);
	gset parametric;
        gset view 0, 0, 1, 1;
	gsplot zz w l 1;
      else
        msg = "contour: rows (z) must be the same as length (x) and";
        msg = sprintf ("%s\ncolumns (z) must be the same as length (y)", msg);
        error (msg);
      endif
    else
      error ("contour: x and y must be vectors and z must be a matrix");
    endif
  else
    usage ("contour (z, levels, x, y)");
  endif

endfunction
