## Copyright (C) 1995, 1996, 1997  Friedrich Leisch
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

## usage:  autocov (X [, h])
## 
## returns the autocovariances from lag 0 to h of vector X.
## If h is omitted, all autocovariances are computed.
## If X is a matrix, the autocovariances of every single column are
## computed. 

## Author:  FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description:  Compute autocovariances
  
function retval = autocov (X, h)
  
  [n, c] = size (X);
  
  if (is_vector (X))
    n = length (X);
    c = 1;
    X = reshape (X, n, 1);
  endif
  
  X = center (X);
  
  if (nargin == 1)
    h = n - 1;
  endif
  
  retval = zeros (h + 1, c);
  
  unwind_protect

    oldpcv = prefer_column_vectors;
    prefer_column_vectors = "false";

    for i = 0 : h
      retval(i+1, :) = diag (X(i+1:n, :).' * conj (X(1:n-i, :))) / n;
    endfor

  unwind_protect_cleanup
    
    prefer_column_vectors = oldpcv;
    
  end_unwind_protect
  
endfunction
