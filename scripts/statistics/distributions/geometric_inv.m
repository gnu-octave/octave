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

## usage:  geometric_inv (x, p)
##
## For each element of x, compute the quantile at x of the geometric
## distribution with parameter p.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Quantile function of the geometric distribution

function inv = geometric_inv (x, p)

  if (nargin != 2)
    usage ("geometric_inv (x, p)");
  endif

  [retval, x, p] = common_size (x, p);
  if (retval > 0)
    error (["geometric_inv:  ", ...
            "x and p must be of common size or scalar"]);
  endif

  [r, c] = size (x);
  s = r * c;
  x   = reshape (x, 1, s);
  p   = reshape (p, 1, s);
  inv = zeros (1, s);

  k = find (!(x >= 0) | !(x <= 1) | !(p >= 0) | !(p <= 1));
  if any (k)
    inv(k) = NaN * ones (1, length (k));
  endif

  k = find ((x == 1) & (p >= 0) & (p <= 1));
  if any (k)
    inv(k) = Inf * ones (1, length (k));
  endif

  k = find ((x > 0) & (x < 1) & (p > 0) & (p <= 1));
  if any (k)
    inv(k) = max (ceil (log (1 - x(k)) ./ log (1 - p(k))) - 1, ...
        zeros (1, length (k)));
  endif

  inv = reshape (inv, r, c);

endfunction
