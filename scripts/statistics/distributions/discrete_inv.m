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

## usage:  discrete_inv (X, V, P)
##
## For each component of X, compute the quantile (the inverse of the
## CDF) at X of the univariate distribution which assumes the values in
## V with probabilities P.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Quantile function of a discrete distribution

function inv = discrete_inv (X, V, P)
  
  if (nargin != 3)
    usage ("discrete_inv (X, V, P)");
  endif

  [r, c] = size (X);

  if (! is_vector (V))
    error ("discrete_inv:  V must be a vector");
  elseif (! is_vector (P) || (length (P) != length (V)))
    error ("discrete_inv:  P must be a vector with length (V) elements");
  elseif (! (all (P >= 0) && any (P)))
    error ("discrete_inv:  P must be a nonzero, nonnegative vector");
  endif

  n = r * c;
  X = reshape (X, 1, n);
  m = length (V);
  [V, ind] = sort (V);
  s = reshape (cumsum (P / sum (P)), m, 1);

  inv = NaN * ones (n, 1);
  if any (k = find (X == 0))
    inv(k) = -Inf * ones (1, length (k));
  endif
  if any (k = find (X == 1))
    inv(k) = V(m) * ones (1, length (k));
  endif
  if any (k = find ((X > 0) & (X < 1)))
    n = length (k);
    ## --FIXME--
    ## This does not work!
    inv(k) = V(sum ((ones (m, 1) * X(k)) > (s * ones (1, n))) + 1);
  endif

  inv = reshape (inv, r, c);

endfunction


