## Copyright (C) 1997 by Vincent Cautaerts
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
## 
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details. 
## 
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## usage: y = fftshift(W)
##
## Performs a shift of the vector V, for use with the "fft" and "ifft"
## functions, in order the move the frequency 0 to the centre of
## the vector or matrix.
##
## If V is a vector of E datas corresponding to E time samples spaced
## of Dt each, then fftshift (fft (V)) correspond to frequencies
##
##   f = linspace (-E/(4*DT), (E/2-1)/(2*DT), E)
##
## If V is a matrix, does the same holds for rows and columns.

## Author: Vincent Cautaerts <vincent@comf5.comm.eng.osaka-u.ac.jp>
## Created: July 1997
## Adapted-By: jwe

function retval = fftshift (V)

  retval = 0;

  if (nargin != 1)
    usage ("usage: fftshift (X)");
  endif

  if (is_vector (V))
    x = length (V);
    xx = ceil (x/2);
    retval = V([xx+1:x, 1:xx]);
  elseif (is_matrix (V))
    [x, y] = size (V);
    xx = ceil (x/2);
    yy = ceil (y/2);
    retval = V([xx+1:x, 1:xx], [yy+1:y, 1:yy]);
  else
    error ("fftshift: expecting vector or matrix argument");
  endif

endfunction
