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

## usage:  cauchy_inv (x [, lambda, sigma])
##
## For each element of x, compute the quantile (the inverse of the CDF)
## at x of the Cauchy distribution with location parameter lambda and
## scale parameter sigma. Default values are lambda = 0, sigma = 1.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Quantile function of the Cauchy distribution

function inv = cauchy_inv (x, location, scale)
  
  if !(nargin == 1 || nargin == 3)
    usage ("cauchy_inv (x [, lambda, sigma])");
  endif
  
  if (nargin == 1)
    location = 0;
    scale = 1;
  endif
  
  [retval, x, location, scale] = common_size (x, location, scale);
  if (retval > 0)
    error (["cauchy_inv:  ", ...
	    "x, lambda and sigma must be of common size or scalar"]);
  endif
  
  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  location = reshape (location, 1, s);
  scale = reshape (scale, 1, s);
  
  inv = NaN * ones (1, s);
  
  ok = ((location > -Inf) & (location < Inf) &
       (scale > 0) & (scale < Inf));
  
  k = find ((x == 0) & ok);
  if any (k)
    inv(k) = -Inf * ones (1, length (k));
  endif
  
  k = find ((x > 0) & (x < 1) & ok);
  if any (k)
    inv(k) = location(k) - scale(k) .* cot (pi * x(k));
  endif
  
  k = find ((x == 1) & ok);
  if any (k)
    inv(k) = Inf * ones (1, length (k));
  endif
  
  inv = reshape (inv, r, c);
  
endfunction
