## Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002, 2003, 2004, 2005,
##               2006, 2007, 2009 Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{pval}, @var{ks}] =} kolmogorov_smirnov_test (@var{x}, @var{dist}, @var{params}, @var{alt})
## Perform a Kolmogorov-Smirnov test of the null hypothesis that the
## sample @var{x} comes from the (continuous) distribution dist.  I.e.,
## if F and G are the CDFs corresponding to the sample and dist,
## respectively, then the null is that F == G.
##
## The optional argument @var{params} contains a list of parameters of
## @var{dist}.  For example, to test whether a sample @var{x} comes from
## a uniform distribution on [2,4], use
##
## @example
## kolmogorov_smirnov_test(x, "uniform", 2, 4)
## @end example
##
## @noindent
## @var{dist} can be any string for which a function @var{dist_cdf}
## that calculates the CDF of distribution @var{dist} exists.
##
## With the optional argument string @var{alt}, the alternative of
## interest can be selected.  If @var{alt} is @code{"!="} or
## @code{"<>"}, the null is tested against the two-sided alternative F
## != G@.  In this case, the test statistic @var{ks} follows a two-sided
## Kolmogorov-Smirnov distribution.  If @var{alt} is @code{">"}, the
## one-sided alternative F > G is considered.  Similarly for @code{"<"},
## the one-sided alternative F > G is considered.  In this case, the
## test statistic @var{ks} has a one-sided Kolmogorov-Smirnov
## distribution.  The default is the two-sided case.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: One-sample Kolmogorov-Smirnov test

function [pval, ks] = kolmogorov_smirnov_test (x, dist, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (! isvector (x))
    error ("kolmogorov_smirnov_test: x must be a vector");
  endif

  n = length (x);
  s = sort (x);
  f = str2func (sprintf ("%s_cdf", dist));

  alt  = "!=";

  if (nargin == 2)
    z = reshape (feval (f, s), 1, n);
  else
    args = "";
    for k = 1 : (nargin-2);
      tmp  = varargin{k};
      if ischar (tmp)
        alt = tmp;
      else
        args = sprintf ("%s, %g", args, tmp);
      endif
    endfor
    z = reshape (eval (sprintf ("%s(s%s);", func2str (f), args)), 1, n);
  endif

  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    ks   = sqrt (n) * max (max ([abs(z - (0:(n-1))/n); abs(z - (1:n)/n)]));
    pval = 1 - kolmogorov_smirnov_cdf (ks);
  elseif (strcmp (alt, ">"))
    ks   = sqrt (n) * max (max ([z - (0:(n-1))/n; z - (1:n)/n]));
    pval = exp (- 2 * ks^2);
  elseif (strcmp (alt, "<"))
    ks   = - sqrt (n) * min (min ([z - (0:(n-1))/n; z - (1:n)/n]));
    pval = exp (- 2 * ks^2);
  else
    error ("kolmogorov_smirnov_test: alternative %s not recognized", alt);
  endif

  if (nargout == 0)
    printf ("pval: %g\n", pval);
  endif

endfunction
