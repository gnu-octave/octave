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
## @deftypefn {Function File} {[@var{pval}, @var{k}, @var{df}] =} kruskal_wallis_test (@var{x1}, @dots{})
## Perform a Kruskal-Wallis one-factor "analysis of variance".
##
## Suppose a variable is observed for @var{k} > 1 different groups, and
## let @var{x1}, @dots{}, @var{xk} be the corresponding data vectors.
##
## Under the null hypothesis that the ranks in the pooled sample are not
## affected by the group memberships, the test statistic @var{k} is
## approximately chi-square with @var{df} = @var{k} - 1 degrees of
## freedom.
##
## The p-value (1 minus the CDF of this distribution at @var{k}) is
## returned in @var{pval}.
##
## If no output argument is given, the p-value is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Kruskal-Wallis test

function [pval, k, df] = kruskal_wallis_test (varargin)

  m = nargin;
  if (m < 2)
    print_usage ();
  endif

  n = [];
  p = [];

  for i = 1 : m;
    x = varargin{i};
    if (! isvector (x))
      error ("kruskal_wallis_test: all arguments must be vectors");
    endif
    l = length (x);
    n = [n, l];
    p = [p, (reshape (x, 1, l))];
  endfor

  r = ranks (p);

  k = 0;
  j = 0;
  for i = 1 : m;
    k = k + (sum (r ((j + 1) : (j + n(i))))) ^ 2 / n(i);
    j = j + n(i);
  endfor

  n    = length (p);
  k    = 12 * k / (n * (n + 1)) - 3 * (n + 1);
  df   = m - 1;
  pval = 1 - chisquare_cdf (k, df);

  if (nargout == 0)
    printf ("pval: %g\n", pval);
  endif

endfunction


