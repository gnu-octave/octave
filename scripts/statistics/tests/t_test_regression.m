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
## @deftypefn {Function File} {[@var{pval}, @var{t}, @var{df}] =} t_test_regression (@var{y}, @var{x}, @var{rr}, @var{r}, @var{alt})
## Perform an t test for the null hypothesis @code{@var{rr} * @var{b} =
## @var{r}} in a classical normal regression model @code{@var{y} =
## @var{x} * @var{b} + @var{e}}.  Under the null, the test statistic @var{t}
## follows a @var{t} distribution with @var{df} degrees of freedom.
##
## If @var{r} is omitted, a value of 0 is assumed.
##
## With the optional argument string @var{alt}, the alternative of
## interest can be selected.  If @var{alt} is @code{"!="} or
## @code{"<>"}, the null is tested against the two-sided alternative
## @code{@var{rr} * @var{b} != @var{r}}.  If @var{alt} is @code{">"}, the
## one-sided alternative @code{@var{rr} * @var{b} > @var{r}} is used.
## Similarly for @var{"<"}, the one-sided alternative @code{@var{rr} *
## @var{b} < @var{r}} is used.  The default is the two-sided case. 
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value of the test is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Test one linear hypothesis in linear regression model

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
  elseif (! (nargin == 5))
    usage ("[pval, t, df] = t_test_regression (y, X, R, r, alt)");
  endif

  if (! isscalar (r))
    error ("t_test_regression: r must be a scalar");
  elseif (! isstr (alt))
    error ("t_test_regression: alt must be a string");
  endif

  [T, k] = size (X);
  if (! (isvector (y) && (length (y) == T)))
    error ("t_test_regression: y must be a vector of length rows (X)");
  endif
  s      = size (R);
  if (! ((max (s) == k) && (min (s) == 1)))
    error ("t_test_regression: R must be a vector of length columns (X)");
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
    printf ("pval: %g\n", pval);
  endif

endfunction
