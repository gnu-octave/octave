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

## usage:  [pval, Tsq] = hotelling_test_2 (x, y)
##
## For two samples x from multivariate normal distributions with the
## same number of variables (columns), unknown means and unknown equal
## covariance matrices, test the null hypothesis mean (x) == mean (y).
##
## Tsq is Hotelling's two-sample T^2.  Under the null,
##    (n_x+n_y-p-1) T^2 / (p(n_x+n_y-2))
## has an F distribution with p and n_x+n_y-p-1 degrees of freedom,
## where n_x and n_y are the sample sizes and p is the number of
## variables.
##
## pval is the p-value of the test.
##
## If no output argument is given, the p-value of the test is displayed.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Compare means of two multivariate normals

function [pval, Tsq] = hotelling_test_2 (x, y)

  if (nargin != 2)
    usage ("hotelling_test_2 (x, y)");
  endif

  if (is_vector (x))
    n_x = length (x);
    if (! is_vector (y))
      error ("hotelling_test_2:  If x is a vector, y must be too.");
    else
      n_y = length (y);
      p   = 1;
    endif
  elseif (is_matrix (x))
    [n_x, p] = size (x);
    [n_y, q] = size (y);
    if (p != q)
      error (strcat ("hotelling_test_2:  ",
                     "x and y must have the same number of columns"));
    endif
  else
    error ("hotelling_test_2:  x and y must be matrices (or vectors)");
  endif

  d    = mean (x) - mean (y);
  S    = ((n_x - 1) * cov (x) + (n_y - 1) * cov (y)) / (n_x + n_y - 2);
  Tsq  = (n_x * n_y / (n_x + n_y)) * d * (S \ d');
  pval = 1 - f_cdf ((n_x + n_y - p - 1) * Tsq / (p * (n_x + n_y - 2)),
                    p, n_x + n_y - p - 1);

  if (nargout == 0)
    printf ("  pval:  %g\n", pval);
  endif

endfunction