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

## usage:  discrete_rnd (N, V, P)
##
## Generate a row vector containing a random sample of size N from the
## univariate distribution which assumes the values in V with
## probabilities P.
##
## Currently, N must be a scalar.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Random deviates from a discrete distribution

function rnd = discrete_rnd (N, V, P)
  
  if (nargin != 3)
    usage ("discrete_rnd (N, V, P)");
  endif

  if (! is_scalar (N))
    error ("discrete_rnd:  N must be a scalar");
  endif

  if (! is_vector (V))
    error ("discrete_rnd:  V must be a vector");
  elseif (! is_vector (P) || (length (P) != length (V)))
    error ("discrete_rnd:  P must be a vector with length (V) elements");
  elseif (! (all (P >= 0) && any (P)))
    error ("discrete_rnd:  P must be a nonzero, nonnegative vector");
  endif

  u = rand (1, N);
  m = length (P);
  s = reshape (cumsum (P / sum (P)), m, 1);

  rnd = V (1 + sum ((s * ones (1, N)) <= ((ones (m, 1) * u))));
  
endfunction
