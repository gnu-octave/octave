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

## usage:  discrete_cdf (X, V, P)
##
## For each element of X, compute the cumulative distribution function
## (CDF) at X of a univariate discrete distribution which assumes the
## values in V with probabilities P.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  CDF of a discrete distribution

function cdf = discrete_cdf (X, V, P)

  if (nargin != 3)
    usage ("discrete_cdf (X, V, P)");
  endif

  [r, c] = size (X);

  if (! is_vector (V))
    error ("discrete_cdf: V must be a vector");
  elseif (! is_vector (P) || (length (P) != length (V)))
    error ("discrete_cdf: P must be a vector with length (V) elements");
  elseif (! (all (P >= 0) && any (P)))
    error ("discrete_cdf: P must be a nonzero, nonnegative vector");
  endif

  n = r * c;
  m = length (V);
  X = reshape (X, n, 1);
  V = reshape (V, 1, m);
  P = reshape (P / sum (P), m, 1);

  cdf = zeros (n, 1);
  k = find (isnan (X));
  if any (k)
    cdf (k) = NaN * ones (length (k), 1);
  endif
  k = find (!isnan (X));
  if any (k)
    n = length (k);
    cdf (k) = ((X(k) * ones (1, m)) >= (ones (n, 1) * V)) * P;
  endif

  cdf = reshape (cdf, r, c);

endfunction
