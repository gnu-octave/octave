## Copyright (C) 1996, 1997  Kurt Hornik
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

## usage:  hypergeometric_pdf (x, m, t, n)
##
## Compute the probability density function (PDF) at x of the
## hypergeometric distribution with parameters m, t, and n. This is the
## probability of obtaining x marked items when randomly drawing a
## sample of size n without replacement from a population of total size
## t containing m marked items.
##
## The arguments must be of common size or scalar.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  PDF of the hypergeometric distribution
  
function pdf = hypergeometric_pdf (x, m, t, n)
  
  if (nargin != 4)
    usage ("hypergeometric_pdf (x, m, t, n)");
  endif
  
  [retval, x, m, t, n] = common_size (x, m, t, n);
  if (retval > 0)
    error (["hypergeometric_pdf:  ", ...
	    "x, m, t, and n must be of common size or scalar"]);
  endif
  
  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  m = reshape (m, 1, s);
  t = reshape (t, 1, s);
  n = reshape (n, 1, s);
  pdf = zeros * ones (1, s);
  ## everything in i1 gives NaN
  i1 = ((m < 0) | (t < 0) | (n <= 0) | (m != round (m)) | 
	(t != round (t)) | (n != round (n)) | (m > t) | (n > t));
  ## everything in i2 gives 0 unless in i1
  i2 = ((x != round (x)) | (x < 0) | (x > m) | (n < x) | (n-x > t-m));
  k = find (i1);
  if any (k)
    pdf (k) = NaN * ones (size (k));
  endif
  k = find (!i1 & !i2);
  if any (k)
    pdf (k) = (bincoeff (m(k), x(k)) .* bincoeff (t(k)-m(k), n(k)-x(k))
	       ./ bincoeff (t(k), n(k)));
  endif
  
endfunction
