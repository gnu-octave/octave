## Copyright (C) 1995, 1996  Kurt Hornik
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

## usage:  erfinv (x)
##
## Computes the inverse of the error function erf.

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 27 September 1994
## Adapted-By: jwe

function y = erfinv (x)
  
  if (nargin != 1)
    usage ("erfinv (x)");
  endif
  
  [m, n] = size (x);  
  x = reshape (x, m * n, 1);
  y = zeros (m * n, 1);
  
  i = find ((x < -1) | (x > 1));
  if any (i)
    y(i) = NaN * ones (length (i), 1);
  endif

  y (find (x == -1)) = (-Inf) * ones (sum (x == -1), 1);
  y (find (x == 1)) = Inf * ones (sum (x == 1), 1);
  
  i = find ((x > -1) & (x < 1));
  if any (i)
    z_old = ones (length (i), 1);
    z_new = zeros (length (i), 1);
    while (any (any (abs (z_new - z_old) > eps)))
      z_old = z_new;
      z_new = z_old - (erf (z_old) - x(i)) .* exp (z_old.^2);
    endwhile
    y(i) = z_new;
  endif
  
  y = reshape (y, m, n);
    
endfunction
