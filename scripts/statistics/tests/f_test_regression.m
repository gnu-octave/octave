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

## usage:  [pval, f, df_num, df_den] = f_test_regression (y, X, R [, r])
##
## Performs an F test for the null hypothesis R * b = r in a classical
## normal regression model y = X * b + e.
##
## Under the null, the test statistic f follows an F distribution with
## df_num and df_den degrees of freedom;  pval is the p-value (1 minus
## the CDF of this distribution at f) of the test.
##
## If not given explicitly, r = 0.
##
## If no output argument is given, the p-value is displayed.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Test linear hypotheses in linear regression model

function [pval, f, df_num, df_den] = f_test_regression (y, X, R, r)
  
  if (nargin < 3 || nargin > 4)
    usage (["[pval, f, df_num, df_den] ", ...
	    "= f_test_regression (y, X, R [, r])"]);
  endif

  [T, k] = size (X);
  if !( is_vector (y) && (length (y) == T) )
    error (["f_test_regression:  ", ...
	    "y must be a vector of length rows (X)."]);
  endif
  y = reshape (y, T, 1);
  
  [q, c_R ] = size (R);
  if (c_R != k)
    error (["f_test_regression:  ", ...
	    "R must have as many columns as X."]);
  endif
  
  if (nargin == 4)
    s_r = size (r);
    if ((min (s_r) != 1) || (max (s_r) != q))
      error (["f_test_regression:  ", ...
	      "r must be a vector of length rows (R)."]); 
    endif
    r = reshape (r, q, 1);
  else
    r = zeros (q, 1);
  endif

  df_num = q;
  df_den = T - k;
  
  [b, v] = ols (y, X);
  diff   = R * b - r;
  f      = diff' * inv (R * inv (X' * X) * R') * diff / ( q * v );
  pval  = 1 - f_cdf (f, df_num, df_den);
  
  if (nargout == 0)
    printf ("  pval:  %g\n", pval);
  endif

endfunction
