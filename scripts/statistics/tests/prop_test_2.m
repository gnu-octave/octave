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

## usage:  [pval, z] = prop_test_2 (x1, n1, x2, n2 [, alt])
##
## If x1 and n1 are the counts of successes and trials in one sample,
## and x2 and n2 those in a second one, test the null hypothesis that
## the success probabilities p1 and p2 are the same.
## Under the null, the test statistic z approximately follows a
## standard normal distribution.
##
## With the optional argument string alt, the alternative of interest
## can be selected.
## If alt is "!=" or "<>", the null is tested against the two-sided
## alternative p1 != p2.
## If alt is ">", the one-sided alternative p1 > p2 is used, similarly
## for "<".
## The default is the two-sided case.
##
## pval is the p-value of the test.
##
## If no output argument is given, the p-value of the test is displayed.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Compare two proportions

function [pval, z] = prop_test_2 (x1, n1, x2, n2, alt)

  if ((nargin < 4) || (nargin > 5))
        usage ("[pval, z] = prop_test_2 (x1, n1, x2, n2 [, alt])");
  endif

  ## Could do sanity checking on x1, n1, x2, n2 here

  p1  = x1 / n1;
  p2  = x2 / n2;
  pc  = (x1 + x2) / (n1 + n2);

  z   = (p1 - p2) / sqrt (pc * (1 - pc) * (1/n1 + 1/n2));

  cdf = stdnormal_cdf (z);

  if (nargin == 4)
    alt  = "!=";
  endif

  if !isstr (alt)
    error ("prop_test_2:  alt must be a string");
  endif
  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif strcmp (alt, ">")
    pval = 1 - cdf;
  elseif strcmp (alt, "<")
    pval = cdf;
  else
    error (sprintf ("prop_test_2:  option %s not recognized", alt));
  endif

  if (nargout == 0)
    printf ("  pval:  %g\n", pval);
  endif

endfunction
