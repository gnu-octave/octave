## Copyright (C) 1995, 1996, 1997  Kurt Hornik
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

## usage:  X = autoreg_matrix (y, k)
##
## Given a time series (vector) y, returns a matrix X with ones in the
## first column and the first k lagged values of y in the other columns.
## I.e., for t > k, [1, y(t-1), ..., y(t-k)] is the t-th row of X. X can
## be used as regressor matrix in autoregressions.
  
## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Design matrix for autoregressions

function X = autoreg_matrix (y, k)

  if (nargin != 2)
    usage ("autoreg_matrix (y, k)");
  endif
  
  if !(is_vector (y))
    error ("autoreg_matrix:  y must be a vector");
  endif
  
  T = length (y);
  y = reshape (y, T, 1);
  X = ones (T, k+1);
  for j = 1 : k;
    X(:, j+1) = [zeros (j, 1); y(1:T-j)];
  endfor
  
endfunction
