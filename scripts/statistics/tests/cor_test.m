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

## usage:  cor_test (X, Y [, ALTERNATIVE [, METHOD]])
##
## Test whether two samples X and Y come from uncorrelated populations.
##
## The optional argument string ALTERNATIVE describes the alternative
## hypothesis, and can be "!=" or "<>" (non-zero), ">" (greater than 0),
## or "<" (less than 0).  The default is the two-sided case.
##
## The optional argument string METHOD specifies on which correlation
## coefficient the test should be based.
## If METHOD is "pearson" (default), the (usual) Pearson's product
## moment correlation coefficient is used.  In this case, the data
## should come from a bivariate normal distribution.  Otherwise, the
## other two methods offer nonparametric alternatives.
## If METHOD is "kendall", then Kendall's rank correlation tau is used.
## If METHOD is "spearman", then Spearman's rank correlation rho is used.
## Only the first character is necessary.
##
## The output is a structure with the following elements:
##      pval            The p-value of the test.
##      stat            The value of the test statistic.
##      dist            The distribution of the test statistic.
##      params          The parameters of the null distribution of the
##                      test statistic.
##      alternative     The alternative hypothesis.
##      method          The method used for testing.
##
## If no output argument is given, the pval is displayed.

## Author:  FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Adapted-by:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Test for zero correlation

function t = cor_test (X, Y, ALTERNATIVE, METHOD)

  if ((nargin < 2) || (nargin > 4))
    usage ("cor_test (X, Y [, ALTERNATIVE [, METHOD]])")
  endif

  if (!is_vector (X) || !is_vector (Y) || length (X) != length (Y))
    error ("cor_test:  X and Y must be vectors of the same length")
  endif

  if (nargin < 3)
    ALTERNATIVE = "!=";
  elseif !isstr (ALTERNATIVE)
    error ("cor_test:  ALTERNATIVE must be a string");
  endif

  if (nargin < 4)
    METHOD = "pearson";
  elseif !isstr (METHOD)
    error ("cor_test:  METHOD must be a string");
  endif

  n = length (X);
  m = METHOD (1);

  if (m == "p")
    r = cor (X, Y);
    df = n - 2;
    t.method = "Pearson's product moment correlation";
    t.params = df;
    t.stat = sqrt (df) .* r / sqrt (1 - r.^2);
    t.dist = "t";
    cdf  = t_cdf (t.stat, df);
  elseif (m == "k")
    tau = kendall (X, Y);
    t.method = "Kendall's rank correlation tau";
    t.params = [];
    t.stat = tau / sqrt ((2 * (2*n+5)) / (9*n*(n-1)));
    t.dist = "stdnormal";
    cdf = stdnormal_cdf (t.stat);
  elseif (m == "s")
    rho = spearman (X, Y);
    t.method = "Spearman's rank correlation rho";
    t.params = [];
    t.stat = sqrt (n-1) * (rho - 6/(n^3-n));
    t.dist = "stdnormal";
    cdf = stdnormal_cdf (t.stat);
  else
    error ("cor_test:  method `%s' not recognized", METHOD)
  endif

  if (strcmp (ALTERNATIVE, "!=") || strcmp (ALTERNATIVE, "<>"))
    t.pval = 2 * min (cdf, 1 - cdf);
  elseif (strcmp (ALTERNATIVE, ">"))
    t.pval = 1 - cdf;
  elseif (strcmp (ALTERNATIVE, "<"))
    t.pval = cdf;
  else
    error ("cor_test:  alternative `%s' not recognized", ALTERNATIVE);
  endif

  t.alternative = ALTERNATIVE;

  if (nargout == 0)
    printf ("pval:  %g\n", t.pval);
  endif

endfunction
