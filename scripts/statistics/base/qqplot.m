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

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{q}, @var{s}] =} qqplot (@var{x}, @var{dist}, @var{params})
## Perform a QQ-plot (quantile plot).
##
## If F is the CDF of the distribution @var{dist} with parameters
## @var{params} and G its inverse, and @var{x} a sample vector of length
## @var{n}, the QQ-plot graphs ordinate @var{s}(@var{i}) = @var{i}-th
## largest element of x versus abscissa @var{q}(@var{i}f) = G((@var{i} -
## 0.5)/@var{n}).
##
## If the sample comes from F except for a transformation of location
## and scale, the pairs will approximately follow a straight line.
##
## The default for @var{dist} is the standard normal distribution.  The
## optional argument @var{params} contains a list of parameters of
## @var{dist}.  For example, for a quantile plot of the uniform
## distribution on [2,4] and @var{x}, use
##
## @example
## qqplot (x, "uniform", 2, 4)
## @end example
##
## If no output arguments are given, the data are plotted directly.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Perform a QQ-plot (quantile plot)

function [q, s] = qqplot (x, dist, ...)

  if (nargin < 1)
    usage ("qqplot (x, dist, params)");
  endif

  if (! (is_vector(x)))
    error ("qqplot: x must be a vector");
  endif

  s = sort (x);
  n = length (x);
  t = ((1 : n)' - .5) / n;
  if (nargin == 1)
    f = "stdnormal_inv";
  else
    f = sprintf ("%s_inv", dist);
  endif;
  if (nargin <= 2)
    q = feval (f, t);
    q_label = f;
  else
    param_string = sprintf ("%g", va_arg ());
    for k = 2 : (nargin - 2);
      param_string = sprintf ("%s, %g", param_string, va_arg ())
    endfor
    q = eval (sprintf ("%s (t, %s);", f, param_string));
    q_label = sprintf ("%s with parameter(s) %s", f, param_string);
  endif

  if (nargout == 0)
    xlabel (q_label);
    ylabel ("sample points");
    set nokey;
    plot (q, s);
  endif

endfunction
