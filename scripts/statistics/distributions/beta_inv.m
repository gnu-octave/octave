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

## usage:  beta_inv (x, a, b)
##
## For each component of x, compute the quantile (the inverse of the
## CDF) at x of the Beta distribution with parameters a and b.
  
## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Quantile function of the Beta distribution

function inv = beta_inv (x, a, b)
  
  if (nargin != 3)
    usage ("beta_inv (x, a, b)");
  endif
  
  [retval, x, a, b] = common_size (x, a, b);
  if (retval > 0)
    error ("beta_inv:  x, a and b must be of common size or scalars");
  endif

  [r, c] = size (x);
  s   = r * c;
  x   = reshape (x, s, 1);
  a   = reshape (a, s, 1);
  b   = reshape (b, s, 1);
  inv = zeros (s, 1);
  
  k = find ((x < 0) | (x > 1) | !(a > 0) | !(b > 0) | isnan (x));
  if any (k)
    inv (k) = NaN * ones (length (k), 1);
  endif
  
  k = find ((x == 1) & (a > 0) & (b > 0));
  if any (k)
    inv (k) = ones (length (k), 1);
  endif
  
  k = find ((x > 0) & (x < 1) & (a > 0) & (b > 0));
  if any (k)
    a = a (k);
    b = b (k);
    x = x (k);
    y = a ./ b;
    l = find (y < eps);
    if any (l)
      y(l) = sqrt (eps) * ones (length (l), 1);
    endif
    l = find (y > 1 - eps);
    if any (l)
      y(l) = 1 - sqrt (eps) * ones (length (l), 1);
    endif

    y_old = y;
    for i = 1 : 100
      h     = (beta_cdf (y_old, a, b) - x) ./ beta_pdf (y_old, a, b);
      y_new = y_old - h;
      ind   = find (y_new <= eps);
      if (any (ind))
	y_new (ind) = y_old (ind) / 10;
      endif
      ind = find (y_new >= 1 - eps);
      if any (ind)
	y_new (ind) = 1 - (1 - y_old (ind)) / 10;
      endif
      h = y_old - y_new;
      if (max (abs (h)) < sqrt (eps))
	break;
      endif
      y_old = y_new;
    endfor
    
    inv (k) = y_new;
  endif

  inv = reshape (inv, r, c);
  
endfunction
