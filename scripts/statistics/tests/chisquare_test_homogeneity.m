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

## usage:  [pval, chisq, df] = chisquare_test_homogeneity (x, y, c)
##
## Given two samples x and y, perform a chisquare test for homogeneity
## of the null hypothesis that x and y come from the same distribution,
## based on the partition induced by the (strictly increasing) entries
## of c.
##
## For large samples, the test statistic chisq approximately follows a
## chisquare distribution with df = length(c) degrees pf freedom. pval
## is the p-value (1 minus the CDF of this distribution at chisq) of the
## test.
##
## If no output argument is given, the p-value is displayed.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Chi-square test for homogeneity

function [pval, chisq, df] = chisquare_test_homogeneity (x, y, c)

  if (nargin != 3)
    usage ("[pval, chisq, df] = chisquare_test_homogeneity (x, y, c)");
  endif

  if (! (is_vector(x) && is_vector(y) && is_vector(c)))
    error ("chisquare_test_homogeneity: x, y and c must be vectors");
  endif
  ## Now test c for strictly increasing entries
  df = length (c);
  if (any ( (c(2 : df) - c(1 : (df - 1))) <= 0))
    error ("chisquare_test_homogeneity:  c must be increasing");
  endif

  c     = [(reshape (c, 1, df)), Inf];
  l_x   = length (x);
  x     = reshape (x, l_x, 1);
  n_x   = sum (x * ones (1, df+1) < ones (l_x, 1) * c);
  l_y   = length (y);
  y     = reshape (y, l_y, 1);
  n_y   = sum(y * ones (1, df+1) < ones (l_y, 1) * c);
  chisq = l_x * l_y * sum ((n_x/l_x - n_y/l_y).^2 ./ (n_x + n_y));
  pval  = 1 - chisquare_cdf (chisq, df);

  if (nargout == 0)
    printf("  pval:  %g\n", pval);
  endif

endfunction
