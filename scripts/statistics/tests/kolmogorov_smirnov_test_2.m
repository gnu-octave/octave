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

## usage:  [pval, ks] = kolmogorov_smirnov_test_2 (x, y [, alt])
##
## Performs a 2-sample Kolmogorov-Smirnov test of the null hypothesis
## that the samples x and y come from the same (continuous) distribution.
## I.e., if F and G are the CDFs corresponding to the x and y samples,
## respectively, then the null is that F == G.
##
## With the optional argument string alt, the alternative of interest
## can be selected.
## If alt is "!=" or "<>", the null is tested against the two-sided
## alternative F != G.  In this case, the test statistic ks follows a
## two-sided Kolmogorov-Smirnov distribution.
## If alt is ">", the one-sided alternative F > G is considered,
## similarly for "<".  In this case, the test statistic ks has a
## one-sided Kolmogorov-Smirnov distribution.
## The default is the two-sided case.
##
## pval is the p-value of the test.
##
## If no output argument is given, the p-value is displayed.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Two-sample Kolmogorov-Smirnov test

function [pval, ks] = kolmogorov_smirnov_test_2 (x, y, alt)

  if (nargin < 2 || nargin > 3)
    usage (strcat ("[pval, ks] = ",
                   "kolmogorov_smirnov_test_2 (x, y [, tol])"));
  endif

  if !( is_vector (x) && is_vector (y))
    error ("kolmogorov_smirnov_test_2:  both x and y must be vectors.");
  endif

  if (nargin == 2)
    alt = "!=";
  else
    if (! isstr (alt))
      error ("kolmogorov_smirnov_test_2:  alt must be a string.");
    endif
  endif

  n_x = length (x);
  n_y = length (y);
  n   = n_x * n_y / (n_x + n_y);
  x   = reshape (x, n_x, 1);
  y   = reshape (y, n_y, 1);
  [s, i] = sort ([x; y]);
  count (find (i <= n_x)) = 1 / n_x;
  count (find (i > n_x)) = - 1 / n_y;
  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    ks   = sqrt (n) * max (abs (cumsum (count)));
    pval = 1 - kolmogorov_smirnov_cdf (ks);
  elseif (strcmp (alt, ">"))
    ks   = sqrt (n) * max (cumsum (count));
    pval = exp(- 2 * ks^2);
  elseif (strcmp(alt, "<"))
    ks   = - sqrt (n) * min (cumsum (count));
    pval = exp(- 2 * ks^2);
  else
    error (sprintf (["kolmogorov_smirnov_test_2:  ", ...
                     "option %s not recognized"], alt));
  endif

  if (nargout == 0)
    printf ("  pval:  %g\n", pval);
  endif

endfunction
