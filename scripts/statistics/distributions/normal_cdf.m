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

## usage:  normal_cdf (x [, m, v])
##
## For each element of x, compute the cumulative distribution function
## (CDF) at x of the normal distribution with mean m and variance v.
##
## Default values are m = 0, v = 1.

## Author:  TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description:  CDF of the normal distribution

function cdf = normal_cdf (x, m, v)

  if !((nargin == 1) || (nargin == 3))
    usage ("normal_cdf (x [, m, v])");
  endif

  if (nargin == 1)
    m = 0;
    v = 1;
  endif

  [retval, x, m, v] = common_size (x, m, v);
  if (retval > 0)
    error (["normal_cdf:  ", ...
            "x, m and v must be of common size or scalars"]);
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  m = reshape (m, 1, s);
  v = reshape (v, 1, s);
  cdf = zeros (1, s);

  k = find (isinf (m) | isnan (m) | !(v >= 0) | !(v < Inf));
  if any (k)
    cdf(k) = NaN * ones (1, length (k));
  endif

  k = find (!isinf (m) & !isnan (m) & (v > 0) & (v < Inf));
  if any (k)
    cdf(k) = stdnormal_cdf ((x(k) - m(k)) ./ sqrt (v(k)));
  endif

  cdf = reshape (cdf, r, c);

endfunction
