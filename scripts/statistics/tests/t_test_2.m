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

## usage:  [pval, t, df] = t_test_2 (x, y [, alt])
##
## For two samples x and y from normal distributions with unknown means
## and unknown equal variances, perform a two-sample t-test of the null
## hypothesis of equal means.
## Under the null, the test statistic t follows a Student distribution
## with df degrees of freedom.
##
## With the optional argument string alt, the alternative of interest
## can be selected.  
## If alt is "!=" or "<>", the null is tested against the two-sided
## alternative mean(x) != mean(y).
## If alt is ">", the one-sided alternative mean(x) > mean(y) is used,
## similarly for "<".  
## The default is the two-sided case.
##
## pval is the p-value of the test.
##  
## If no output argument is given, the p-value of the test is displayed.
  
## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Student's two-sample t test

function [pval, t, df] = t_test_2 (x, y, alt)
  
  if ((nargin < 2) || (nargin > 3))
	usage ("[pval, t, df] = t_test_2 (x, y [, alt])");
  endif
    
  if (! (is_vector (x) && is_vector (y)))
    error ("t_test_2:  both x and y must be vectors");
  endif

  n_x  = length (x);
  n_y  = length (y);
  df   = n_x + n_y - 2;
  mu_x = sum (x) / n_x;
  mu_y = sum (y) / n_y;
  v    = sumsq (x - mu_x) + sumsq (y - mu_y);
  t    = (mu_x - mu_y) * sqrt ( (n_x * n_y * df) / (v * (n_x + n_y)) );
  cdf  = t_cdf (t, df);

  if (nargin == 2)
    alt = "!=";
  endif

  if (! isstr (alt))
    error ("t_test_2:  alt must be a string");
  endif
  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif strcmp (alt, ">")
    pval = 1 - cdf;
  elseif strcmp (alt, "<")
    pval = cdf;
  else
    error (sprintf ("t_test_2:  option %s not recognized", alt));
  endif

  if (nargout == 0)
    printf ("  pval:  %g\n", pval);
  endif

endfunction
