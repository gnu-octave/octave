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
## @deftypefn {Function File} {} t_cdf (@var{x}, @var{n})
## For each element of @var{x}, compute the CDF at @var{x} of the
## t (Student) distribution with @var{n} degrees of freedom, i.e.,
## PROB (t(@var{n}) <= @var{x}).
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: CDF of the t distribution

function cdf = t_cdf (x, n)

  if (nargin != 2)
    usage ("t_cdf (x, n)");
  endif

  [retval, x, n] = common_size (x, n);
  if (retval > 0)
    error ("t_cdf: x and n must be of common size or scalar");
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  n = reshape (n, 1, s);
  cdf = zeros (1, s);

  k = find (isnan (x) | !(n > 0));
  if (any (k))
    cdf(k) = NaN * ones (1, length (k));
  endif

  k = find ((x == Inf) & (n > 0));
  if (any (k))
    cdf(k) = ones (1, length (k));
  endif

  k = find ((x > -Inf) & (x < Inf) & (n > 0));
  if (any (k))
    cdf(k) = betai (n(k) / 2, 1 / 2, 1 ./ (1 + x(k) .^ 2 ./ n(k))) / 2;
    ind = find (x(k) > 0);
    if (any (ind))
      cdf(k(ind)) = 1 - cdf(k(ind));
    endif
  endif

  ## should we really only allow for positive integer n?
  k = find (n != round (n));
  if (any (k))
    warning ("t_cdf: n should be positive integer");
    cdf(k) = NaN * ones (1, length (k));
  endif

  cdf = reshape (cdf, r, c);

endfunction
