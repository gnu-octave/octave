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

## usage:  [pval, t, df] = t_test_regression (y, X, R [, r] [, alt])
##
## Performs an t test for the null hypothesis R * b = r in a classical
## normal regression model y = X * b + e.
## Under the null, the test statistic t follows a t distribution with
## df degrees of freedom.
##
## r is taken as 0 if not given explicitly.
##
## With the optional argument string alt, the alternative of interest
## can be selected.
## If alt is "!=" or "<>", the null is tested against the two-sided
## alternative R * b != r.
## If alt is ">", the one-sided alternative R * b > r is used,
## similarly for "<".
## The default is the two-sided case.
##
## pval is the p-value of the test.
##
## If no output argument is given, the p-value of the test is displayed.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Test one linear hypothesis in linear regression model

function [pval, t, df] = t_test_regression (y, X, R, r, alt)

  if (nargin == 3)
    r   = 0;
    alt = "!=";
  elseif (nargin == 4)
    if (isstr (r))
      alt = r;
      r   = 0;
    else
      alt = "!=";
    endif
  elseif !(nargin == 5)
    usage (["[pval, t, df] ", ...
            "= t_test_regression (y, X, R [, r] [, alt]"]);
  endif

  if (! is_scalar (r))
    error ("t_test_regression:  r must be a scalar");
  elseif (! isstr (alt))
    error ("t_test_regression:  alt must be a string");
  endif

  [T, k] = size (X);
  if !(is_vector (y) && (length (y) == T))
    error (["t_test_regression:  ", ...
            "y must be a vector of length rows (X)"]);
  endif
  s      = size (R);
  if !((max (s) == k) && (min (s) == 1))
    error (["t_test_regression:  ", ...
            "R must be a vector of length columns (X)"]);
  endif

  R      = reshape (R, 1, k);
  y      = reshape (y, T, 1);
  [b, v] = ols (y, X);
  df     = T - k;
  t      = (R * b - r) / sqrt (v * R * inv (X' * X) * R');
  cdf    = t_cdf (t, df);

  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif strcmp (alt, ">")
    pval = 1 - cdf;
  elseif strcmp (alt, "<")
    pval = cdf;
  else
    error ("t_test_regression: the value `%s' for alt is not possible", alt);
  endif

  if (nargout == 0)
    printf ("pval:  %g\n", pval);
  endif

endfunction
