### Copyright (C) 1996 John W. Eaton
###
### This file is part of Octave.
###
### Octave is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2, or (at your option)
### any later version.
###
### Octave is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with Octave; see the file COPYING.  If not, write to the Free
### Software Foundation, 59 Temple Place - Suite 330, Boston, MA
### 02111-1307, USA.

## usage: [BETA, v [,R]] = gls (Y, X, O)
##
## Generalized Least Squares (GLS) estimation for the multivariate model
##
##   Y = X*B + E,  mean(E) = 0,  cov(vec(E)) = (s^2)*O
##
## with Y ...  T x p      As usual, each row of Y and X is an observation
##      X ...  T x k      and each column a variable.
##      B ...  k x p
##      E ...  T x p
##      O ... Tp x Tp.
##
## BETA is the GLS estimator for B.
## v is the GLS estimator for s^2.
## R = Y - X*BETA is the matrix of GLS residuals.

function [BETA, v, R] = gls (Y, X, O)
  
  ## Written by Teresa Twaroch (twaroch@ci.tuwien.ac.at) May 1993.
  ## Dept of Probability Theory and Statistics TU Wien, Austria.

  if (nargin != 3)
    usage ("[BETA, v [, R]] = gls (Y, X, O)");
  endif

  [rx, cx] = size (X);
  [ry, cy] = size (Y);
  if (rx != ry)
    error ("gls: incorrect matrix dimensions");  
  endif

  O = O^(-1/2);
  Z = kron (eye (cy), X);
  Z = O * Z;
  Y1 = O * reshape (Y, ry*cy, 1);
  U = Z' * Z;
  r = rank (U);

  if (r == cx*cy)
    B = inv (U) * Z' * Y1;
  else
    B = pinv (Z) * Y1;
  endif

  BETA = reshape (B, cx, cy);
  R = Y - X * BETA;
  v = (reshape (R, ry*cy, 1))' * (O^2) * reshape (R, ry*cy, 1) / (rx*cy - r);

endfunction
