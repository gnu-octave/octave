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

## usage:  [pval, z] = z_test_2 (x, y, v_x, v_y [, alt])
##
## For two samples x and y from normal distributions with unknown
## means and known variances v_x and v_y, perform a Z-test of the
## hypothesis of equal means.
## Under the null, the test statistic z follows a standard normal
## distribution.
##
## With the optional argument string alt, the alternative of interest
## can be selected.
## If alt is "!=" or "<>", the null is tested against the two-sided
## alternative mean(x) != mean(y).
## If alt is ">", the one-sided alternative mean(x) > mean(y) is
## used, similarly for "<".
## The default is the two-sided case.
##
## pval is the p-value of the test.
##
## If no output argument is given, the p-value of the test is displayed
## along with some information.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Compare means of two normal samples with known variances

function [pval, z] = z_test_2 (x, y, v_x, v_y, alt)

  if ((nargin < 4) || (nargin > 5))
    usage ("[pval, z] = z_test_2 (x, y, v_x, v_y [, alt])");
  endif

  if (! (is_vector (x) && is_vector (y)))
    error("z_test_2:  both x and y must be vectors");
  elseif (! (is_scalar (v_x) && (v_x > 0)
             && is_scalar (v_y) && (v_y > 0)))
    error ("z_test_2:  both v_x and v_y must be positive scalars.");
  endif

  n_x  = length (x);
  n_y  = length (y);
  mu_x = sum (x) / n_x;
  mu_y = sum (y) / n_y;
  z    = (mu_x - mu_y) / sqrt (v_x / n_x + v_y / n_y);
  cdf  = stdnormal_cdf (z);

  if (nargin == 4)
    alt = "!=";
  endif

  if (! isstr (alt))
    error ("z_test_2:  alt must be a string");
  elseif (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif (strcmp (alt, ">"))
    pval = 1 - cdf;
  elseif (strcmp (alt, "<"))
    pval = cdf;
  else
    error (sprintf ("z_test_2:  option %s not recognized", alt));
  endif

  if (nargout == 0)
    s = strcat ("Two-sample Z-test of mean(x) == mean(y) against ",
                "mean(x) %s mean(y),\n",
                "with known var(x) == %g and var(y) == %g:\n",
                "  pval = %g\n");
    printf (s, alt, v_x, v_y, pval);
  endif

endfunction
