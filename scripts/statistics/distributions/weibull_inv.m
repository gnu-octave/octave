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

## usage:  weibull_inv (x, lambda, alpha)
##
## Compute the quantile (the inverse of the CDF) at x of the Weibull
## distribution with shape parameter alpha and scale parameter sigma.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Quantile function of the Weibull distribution

function inv = weibull_inv (x, shape, scale)

  if (nargin != 3)
    usage ("weibull_inv (x, alpha, sigma)");
  endif

  [retval, x, shape, scale] = common_size (x, shape, scale);
  if (retval > 0)
    error (["weibull_inv:  ", ...
            "x, alpha and sigma must be of common size or scalar"]);
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  shape = reshape (shape, 1, s);
  scale = reshape (scale, 1, s);

  inv = NaN * ones (1, s);
  ok = ((shape > 0) & (shape < Inf) & (scale > 0) & (scale < Inf));

  k = find ((x == 0) & ok);
  if any (k)
    inv(k) = -Inf * ones (1, length (k));
  endif

  k = find ((x > 0) & (x < 1) & ok);
  if any (k)
    inv(k) = scale(k) .* (- log (1 - x(k))) .^ (1 ./ shape(k));
  endif

  k = find ((x == 1) & ok);
  if any (k)
    inv(k) = Inf * ones (1, length (k));
  endif

  inv = reshape (inv, r, c);

endfunction
