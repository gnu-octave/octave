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

## usage: gammai (a, x)
##
## Computes the incomplete gamma function
##
##    gammai(a, x) 
##      = (integral from 0 to x of exp(-t) t^(a-1) dt) / gamma(a).
##
## If a is scalar, then gammai(a, x) is returned for each element of x
## and vice versa.
##
## If neither a nor x is scalar, the sizes of a and x must agree, and
## gammai is applied pointwise.

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 13 August 1994
## Adapted-By: jwe

function y = gammai (a, x)
  
  if (nargin != 2)
    usage ("gammai (a, x)");
  endif
  
  [retval, a, x] = common_size (a, x);
  if (retval > 0)
    error ("gammai:  a and x must be of common size or scalar");
  endif
  
  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  a = reshape (a, 1, s);
  y = zeros (1, s);

  k = find (!(a > 0) | isnan (x));
  if any (k)
    y(k) = NaN * ones (1, length (k));
  endif
  
  k = find ((x == Inf) & (a > 0));
  if any (k)
    y(k) = ones (1, length (k));
  endif
  
  ## For x < a + 1, use summation.  The below choice of L should ensure
  ## that the overall error is less than eps ... 
  k = find((x > 0) & (x < a + 1));
  if any (k)
    L = ceil (- max ([a(k), x(k)]) * log (eps));
    A = cumprod ((ones (L, 1) * x(k)) ...
	./ (ones (L, 1) * a(k) + (1 : L)' * ones (1, length (k))));
    y(k) = exp (-x(k) + a(k) .* log (x(k))) ...
	.* (1 + sum (A)) ./ gamma (a(k) + 1);
  endif

  ## For x >= a + 1, use the continued fraction.
  ## Note, however, that this converges MUCH slower than the series
  ## expansion for small a and x not too large!
  k = find ((x >= a + 1) & (x < Inf) & (a > 0));
  if any (k)
    len = length (k);
    t0  = zeros (1, len);
    t1  = ones (1, len);
    u   = [t0; t1];
    v   = [t1; x(k)];
    c_old = 0;
    c_new = v(1, :) ./ v(2, :);
    n   = 1;
    while (max (abs (c_old ./ c_new - 1)) > 10 * eps)
      c_old = c_new;
      u = v + u .* (ones (2, 1) * (n - a(k)));
      v = u .* (ones (2, 1) * x(k)) + n * v;
      c_new = v(1, :) ./ v(2, :);
      n = n + 1;
    endwhile
    y(k) = 1 - exp (-x(k) + a(k) .* log (x(k))) .* c_new ...
	./ gamma (a(k));
  endif
  
  y = reshape (y, r, c);

endfunction
