## Copyright (C) 1996 John W. Eaton
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

## usage: [xx, yy] = meshgrid (x, y)
##
## Given vectors of x and y coordinates, return two matrices corresponding
## to the x and y coordinates of a mesh.  The rows of xx are copies of x,
## and the columns of yy are copies of y.
##
## [xx, yy] = meshgrid (x) is an abbreviation for [xx, yy] = meshgrid (x, x).
##
## See also: plot, semilogx, semilogy, loglog, polar, mesh, meshdom, contour,
##           bar, stairs, gplot, gsplot, replot, xlabel, ylabel, title 

## Author: jwe

function [xx, yy] = meshgrid (x, y)

  if (nargin == 1)
    y = x;
  endif
  if (nargin > 0 && nargin < 3)
    if (is_vector (x) && is_vector (y))
      xlen = length (x);
      ylen = length (y);
      xx = zeros (ylen, xlen);
      yy = zeros (ylen, xlen);
      if (columns (x) == 1)
        x = x';
      endif
      if (rows (y) == 1)
        y = y';
      endif
      for i = 1:ylen
        xx(i,:) = x;
      endfor
      for i = 1:xlen
        yy(:,i) = y;
      endfor
    else
      error ("meshgrid: arguments must be vectors");
    endif
  else
    usage ("[xx, yy] = meshgrid (x, y)");
  endif

endfunction
