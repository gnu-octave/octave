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

## usage:  pascal_cdf (x, n, p)
##
## For each element of x, compute the CDF at x of the Pascal (negative
## binomial) distribution with parameters n and p.
##
## The number of failures in a Bernoulli experiment with success
## probability p before the n-th success follows this distribution.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  CDF of the Pascal (negative binomial) distribution

function cdf = pascal_cdf (x, n, p)

  if (nargin != 3)
    usage ("pascal_cdf (x, n, p)");
  endif

  [retval, x, n, p] = common_size (x, n, p);
  if (retval > 0)
    error (["pascal_cdf:  ", ...
            "x, n and p must be of common size or scalar"]);
  endif

  [r, c] = size (x);
  s = r * c;
  x   = reshape (x, 1, s);
  n   = reshape (n, 1, s);
  p   = reshape (p, 1, s);
  cdf = zeros (1, s);

  k = find (isnan (x) | (n < 1) | (n == Inf) | (n != round (n)) ...
      | (p < 0) | (p > 1));
  if any (k)
    cdf(k) = NaN * ones (1, length (k));
  endif

  k = find ((x == Inf) & (n > 0) & (n < Inf) & (n == round (n)) ...
      & (p >= 0) & (p <= 1));
  if any (k)
    cdf(k) = ones (1, length (k));
  endif

  k = find ((x >= 0) & (x < Inf) & (x == round (x)) & (n > 0) ...
      & (n < Inf) & (n == round (n)) & (p > 0) & (p <= 1));
  if any (k)
    ## Does anyone know a better way to do the summation?
    m = zeros (1, length (k));
    x = floor (x(k));
    n = n(k);
    p = p(k);
    y = cdf(k);
    while (1)
      l = find (m <= x);
      if any (l)
        y(l) = y(l) + pascal_pdf (m(l), n(l), p(l));
        m(l) = m(l) + 1;
      else
        break;
      endif
    endwhile
    cdf(k) = y;
  endif

  cdf = reshape (cdf, r, c);

endfunction
