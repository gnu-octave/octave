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

## usage:  [pval, Tsq] = hotelling_test (x, m)
##
## For a sample x from a multivariate normal distribution with unknown
## mean and covariance matrix, test the null hypothesis that mean (x) ==
## m.
##
## Tsq is Hotelling's T^2.  Under the null, (n-p) T^2 / (p(n-1)) has an
## F distribution with p and n-p degrees of freedom, where n and p are
## the numbers of samples and variables, respectively.
##
## pval is the p-value of the test.
##
## If no output argument is given, the p-value of the test is displayed.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Test for mean of a multivariate normal

function [pval, Tsq] = hotelling_test (x, m)

  if (nargin != 2)
    usage ("hotelling_test (x, m)");
  endif

  if (is_vector (x))
    if (! is_scalar (m))
      error ("hotelling_test:  If x is a vector, m must be a scalar.");
    endif
    n = length (x);
    p = 1;
  elseif (is_matrix (x))
    [n, p] = size (x);
    if (n <= p)
      error ("hotelling_test:  x must have more rows than columns.");
    endif
    if (is_vector (m) && length (m) == p)
      m = reshape (m, 1, p);
    else
      error (strcat ("hotelling_test:  ",
                     "If x is a matrix, m must be a vector\n",
                     "\tof length columns (x)"));
    endif
  else
    error ("hotelling_test:  x must be a matrix or vector");
  endif

  d    = mean (x) - m;
  Tsq  = n * d * (cov (x) \ d');
  pval = 1 - f_cdf ((n-p) * Tsq / (p * (n-1)), p, n-p);

  if (nargout == 0)
    printf ("  pval:  %g\n", pval);
  endif

endfunction
