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

## usage:  [pval, b, n] = sign_test (x, y [, alt])
##
## For two matched-pair samples x and y, perform a sign test of the
## null hypothesis PROB(x > y) == PROB(x < y) == 1/2.
## Under the null, the test statistic b roughly follows a binomial
## distribution with parameters n = sum (x != y) and p = 1/2.
##
## With the optional argument alt, the alternative of interest can be
## selected.
## If alt is "!=" or "<>", the null hypothesis is tested against the
## two-sided alternative PROB(x < y) != 1/2.
## If alt is ">", the one-sided alternative PROB(x > y) > 1/2 ("x is
## stochastically greater than y") is considered, similarly for "<".
## The default is the two-sided case.
##
## pval is the p-value of the test.
##
## If no output argument is given, the p-value of the test is displayed.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Sign test

function [pval, b, n] = sign_test (x, y, alt)

  if ((nargin < 2) || (nargin > 3))
    usage ("[pval, b, n] = sign_test (x, y [, alt])");
  endif

  if (! (is_vector (x) && is_vector (y) && (length (x) == length (y))))
    error ("sign_test:  x and y must be vectors of the same length");
  endif

  n   = length (x);
  x   = reshape (x, 1, n);
  y   = reshape (y, 1, n);
  n   = sum (x != y);
  b   = sum (x > y);
  cdf = binomial_cdf (b, n, 1/2);

  if (nargin == 2)
    alt  = "!=";
  endif

  if (! isstr (alt))
    error ("sign_test:  alt must be a string");
  endif
  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif strcmp (alt, ">")
    pval = 1 - cdf;
  elseif strcmp (alt, "<")
    pval = cdf;
  else
    error (sprintf ("sign_test:  option %s not recognized", alt));
  endif

  if (nargout == 0)
    printf ("  pval:  %g\n", pval);
  endif

endfunction
