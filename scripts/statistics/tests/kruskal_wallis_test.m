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

## usage:  [pval, k, df] = kruskal_wallis_test (x1, ...)
##
## Perform a Kruskal-Wallis one-factor "analysis of variance".
##
## Suppose a variable is observed for k > 1 different groups, and let
## x1, ..., xk be the corresponding data vectors.
##
## Under the null hypothesis that the ranks in the pooled sample are not
## affected by the group memberships, the test statistic k is
## approximately chi-square with df = k - 1 degrees of freedom. pval is
## the p-value (1 minus the CDF of this distribution at k) of this test.
##
## If no output argument is given, the pval is displayed.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Kruskal-Wallis test
  
function [pval, k, df] = kruskal_wallis_test (...)
  
  m = nargin;
  if (m < 2)
    usage ("[pval, k, df] = kruskal_wallis_test (x1, ...)");
  endif
  
  n = [];
  p = [];
  va_start;
  for i = 1 : m;
    x = va_arg ();
    if (! is_vector (x))
      error ("kruskal_wallis_test:  all arguments must be vectors");
    endif
    l = length (x);
    n = [n, l];
    p = [p, reshape (x, 1, l)];
  endfor
  
  r = ranks (p);

  k = 0;
  j = 0;
  for i = 1 : m;
    k = k + (sum (r ((j + 1) : (j + n(i))))) ^ 2 / n(i);
    j = j + n(i);
  endfor
  
  n    = length (p);
  k    = 12 * k / (n * (n + 1)) - 3 * (n + 1);
  df   = m - 1;
  pval = 1 - chisquare_cdf (k, df);
  
  if (nargout == 0)
    printf ("pval:  %g\n", pval);
  endif

endfunction


