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

## usage: betai (a, b, x)
##
## Returns the incomplete beta function
##
##   betai (a, b, x) = BETA(a,b)^(-1) INT_0^x t^(a-1) (1-t)^(b-1) dt.

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 2 August 1994
## Adapted-By: jwe

## Computation is based on the series expansion
##   betai(a, b, x) 
##     = \frac{x^a}{B(a, b)}
##         \sum_{l=0}^\infty \frac{(1-b)\cdots(l-b)}{a+l} \frac{x^l}{l!}
## for x <= 1/2.  For x > 1/2, betai(a, b, x) = 1 - betai(b, a, 1-x).

function y = betai (a, b, x)
  
  if (nargin != 3)
    usage ("betai (a, b, x)");
  endif

  [retval, x, a, b] = common_size (x, a, b);
  if (retval > 0)
    error ("betai:  a, b and x must be of common size or scalar");
  endif
  
  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  a = reshape (a, 1, s);
  b = reshape (b, 1, s);
  y = zeros (1, s);

  k = find ((x < 0) | (x > 1) | !(a > 0) | !(b > 0) | isnan (x));
  if any (k)
    y(k) = NaN * ones (1, length (k));
  endif

  k = find ((x == 1) & (a > 0) & (b > 0));
  if any (k)
    y(k) = ones (1, length (k));
  endif

  ## Now do the series computations.  
  ## The error when truncating at term L is always less than 2^(-L),
  ## hence the following choice of L. 

  L = ceil (-log (eps) / log (2));
  h = ones (L, 1);
  
  k = find ((x >= 0) & (x <= 1/2) & (a > 0) & (b > 0));
  if any (k)
    l   = (1 : L)' * ones (1, length (k));
    tmp = cumprod ((1 - (h * b(k)) ./ l) .* (h * x(k))) ...
	./ ((h * a(k)) + l);
    y(k) = exp (a(k) .* log (x(k))) .* (1 ./ a(k) + sum (tmp)) ...
	./ beta (a(k), b(k));
  endif
  
  k = find ((x > 1/2) & (x < 1) & (a > 0) & (b > 0));
  if any (k)
    l   = (1 : L)' * ones (1, length (k));
    tmp = cumprod ((1 - (h * a(k)) ./ l) .* (h * (1 - x(k)))) ...
	./ (h * b(k) + l);
    y(k) = 1 - exp (b(k) .* log (1 - x(k))) ...
	.* (1 ./ b(k) + sum (tmp)) ./ beta (a(k), b(k));
  endif

  y = reshape (y, r, c);
  
endfunction





