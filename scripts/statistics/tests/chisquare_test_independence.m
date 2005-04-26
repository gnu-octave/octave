## Copyright (C) 1995, 1996, 1997  Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{pval}, @var{chisq}, @var{df}] =} chisquare_test_independence (@var{x})
## Perform a chi-square test for indepence based on the contingency
## table @var{x}.  Under the null hypothesis of independence,
## @var{chisq} approximately has a chi-square distribution with
## @var{df} degrees of freedom.
##
## The p-value (1 minus the CDF of this distribution at chisq) of the
## test is returned in @var{pval}.
##
## If no output argument is given, the p-value is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Chi-square test for independence

function [pval, chisq, df] = chisquare_test_independence (X)

  if (nargin != 1)
    usage ("chisquare_test_independence (X)");
  endif

  [r, s] = size (X);
  df = (r - 1) * (s - 1);
  n = sum (sum (X));
  Y = sum (X')' * sum (X) / n;
  X = (X - Y) .^2 ./ Y;
  chisq = sum (sum (X));
  pval  = 1 - chisquare_cdf (chisq, df);

  if (nargout == 0)
    printf("  pval: %g\n", pval);
  endif

endfunction