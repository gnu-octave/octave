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
## @deftypefn {Function File} {} binomial_cdf (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the CDF at @var{x} of the
## binomial distribution with parameters @var{n} and @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: CDF of the binomial distribution

function cdf = binomial_cdf (x, n, p)

  if (nargin != 3)
    usage ("binomial_cdf (x, n, p)");
  endif

  [retval, x, n, p] = common_size (x, n, p);
  if (retval > 0)
    error ("binomial_cdf: x, n and p must be of common size or scalar");
  endif

  [r, c] = size (x);
  s = r * c;
  x   = reshape (x, 1, s);
  n   = reshape (n, 1, s);
  p   = reshape (p, 1, s);
  cdf = zeros (1, s);

  k = find (isnan (x) | !(n >= 0) | (n != round (n))
	    | !(p >= 0) | !(p <= 1));
  if (any (k))
    cdf(k) = NaN * ones (1, length (k));
  endif

  k = find ((x >= n) & (n >= 0) & (n == round (n))
	    & (p >= 0) & (p <= 1));
  if (any (k))
    cdf(k) = ones (1, length (k));
  endif

  k = find ((x >= 0) & (x < n) & (n == round (n))
	    & (p >= 0) & (p <= 1));
  if (any (k))
    tmp = floor (x(k));
    cdf(k) = 1 - betai (tmp + 1, n(k) - tmp, p(k));
  endif

  cdf = reshape (cdf, r, c);

endfunction
