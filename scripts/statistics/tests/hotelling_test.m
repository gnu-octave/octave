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

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{pval}, @var{Tsq}] =} hotelling_test (@var{x}, @var{m})
## For a sample @var{x} from a multivariate normal distribution with unknown
## mean and covariance matrix, test the null hypothesis that @code{mean
## (@var{x}) == @var{m}}.
##
## Hotelling's T^2 is returned in @var{Tsq}.  Under the null,
## @math{(n-p) T^2 / (p(n-1))} has an F distribution with @math{p} and
## @math{n-p} degrees of freedom, where @math{n} and @math{p} are the
## numbers of samples and variables, respectively.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value of the test is displayed.
## @end deftypefn

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
