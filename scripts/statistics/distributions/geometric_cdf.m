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

## usage:  geometric_cdf (x, p)
##
## For each element of x, compute the CDF at x of the geometric
## distribution with parameter p.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  CDF of the geometric distribution

function cdf = geometric_cdf (x, p)

  if (nargin != 2)
    usage ("geometric_cdf (x, p)");
  endif

  [retval, x, p] = common_size (x, p);
  if (retval > 0)
    error (["geometric_cdf:  ", ...
            "x and p must be of common size or scalar"]);
  endif

  [r, c] = size (x);
  s = r * c;
  x   = reshape (x, 1, s);
  p   = reshape (p, 1, s);
  cdf = zeros (1, s);

  k = find (isnan (x) | !(p >= 0) | !(p <= 1));
  if any (k)
    cdf(k) = NaN * ones (1, length (k));
  endif

  k = find ((x == Inf) & (p >= 0) & (p <= 1));
  if any (k)
    cdf(k) = ones (1, length (k));
  endif

  k = find ((x >= 0) & (x < Inf) & (x == round (x)) ...
      & (p > 0) & (p <= 1));
  if any (k)
    cdf(k) = 1 - ((1 - p(k)) .^ (x(k) + 1));
  endif

  cdf = reshape (cdf, r, c);

endfunction
