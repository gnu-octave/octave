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

## usage:  [pval, lm] = arch_test (y, X, p)
##         [pval, lm] = arch_test (y, k, p)
##
## arch_test (y, X, p) performs a Lagrange Multiplier (LM) test of the
## null hypothesis of no conditional heteroscedascity in the linear
## regression model y = X * b + e against the alternative of CH(p).   
## I.e., the model is
##     y(t) = b(1) * x(t,1) + ... + b(k) * x(t,k) + e(t),
## where given y up to t-1 and x up to t, e(t) is N(0, h(t)) with
##     h(t) = v + a(1) * e(t-1)^2 + ... + a(p) * e(t-p)^2,
## and the null is a(1) == ... == a(p) == 0.
##
## arch_test (y, k, p) does the same in a linear autoregression model of
## order k, i.e., with [1, y(t-1), ..., y(t-k)] as the t-th row of X. 
##
## Under the null, lm approximately has a chisquare distribution with p
## degrees of freedom.  pval is the p-value (1 minus the CDF of this
## distribution at lm) of the test.
##
## If no output argument is given, the p-value is displayed.
  
## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Test for conditional heteroscedascity
  
function [pval, lm] = arch_test (y, X, p)

  if (nargin != 3)
    error ("arch_test needs 3 input arguments");
  endif

  if !(is_vector (y))
    error ("arch_test:  y must be a vector");
  endif
  T   = length (y);
  y   = reshape (y, T, 1);
  [rx, cx] = size (X);
  if ((rx == 1) && (cx == 1))
    X = autoreg_matrix (y, X);
  elseif !(rx == T)
    error (["arch_test:  ", ...
	    "either rows(X) == length(y), or X is a scalar"]);
  endif
  if !(is_scalar(p) && (rem(p, 1) == 0) && (p > 0))
    error ("arch_test:  p must be a positive integer.");
  endif
  
  [b, v_b, e] = ols (y, X);
  Z    = autoreg_matrix (e.^2, p);
  f    = e.^2 / v_b - ones (T, 1);
  f    = Z' * f;
  lm   = f' * inv (Z'*Z) * f / 2;
  pval = 1 - chisquare_cdf (lm, p);
  
endfunction