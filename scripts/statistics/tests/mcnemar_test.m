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

## usage:  [pval, chisq, df] = mcnemar_test (x)
##
## For a square contingency table x of data cross-classified on the row
## and column variables, McNemar's test can be used for testing the null
## hypothesis of symmetry of the classification probabilities.
##
## Under the null, chisq is approximately distributed as chisquare with
## df degrees of freedom, and pval is the p-value (1 minus the CDF of
## this distribution at chisq) of the test.
##
## If no output argument is given, the p-value of the test is displayed.
  
## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  McNemar's test for symmetry
  
function [pval, chisq, df] = mcnemar_test (x)
  
  if (nargin != 1)
    usage ("mcnemar_test (x)");
  endif
  
  if (! (min (size (x)) > 1) && is_square (x))
    error (strcat ("mcnemar_test:  ",
		   "x must be a square matrix of size > 1."));
  elseif (! (all (all (x >= 0)) && all (all (x == round (x)))))
    error (strcat ("mcnemar_test:  ",
		   "all entries of x must be nonnegative integers."));
  endif
  
  r = rows (x);
  df = r * (r - 1) / 2;
  if (r == 2)
    num = max (abs (x - x') - 1, 0) .^ 2;
  else
    num = abs (x - x') .^ 2;
  endif
  
  chisq = sum (sum (triu (num ./ (x + x'), 1)));
  pval = 1 - chisquare_cdf (chisq, df);
  
  if (nargout == 0)
    printf ("  pval:  %g\n", pval);
  endif
  
endfunction
  


