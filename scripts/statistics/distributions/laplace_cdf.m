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

## usage:  laplace_cdf (x)
##
## For each element of x, compute the cumulative distribution function
## (CDF) at x of the Laplace distribution.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  CDF of the Laplace distribution

function cdf = laplace_cdf (x)

  if (nargin != 1)
    usage ("laplace_cdf (x)");
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  cdf = zeros (1, s);

  k = find (isnan (x));
  if any (k)
    cdf(k) = NaN * ones (1, length (k));
  endif

  k = find (x == Inf);
  if any (k)
    cdf(k) = ones (1, length (k));
  endif

  k = find ((x > -Inf) & (x < Inf));
  if any (k)
    cdf(k) = (1 + sign (x(k)) .* (1 - exp (- abs (x(k))))) / 2;
  endif

  cdf = reshape (cdf, r, c);

endfunction