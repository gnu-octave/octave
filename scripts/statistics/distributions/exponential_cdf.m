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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} exponential_cdf (@var{x}, @var{lambda})
## For each element of @var{x}, compute the cumulative distribution
## function (CDF) at @var{x} of the exponential distribution with
## parameter @var{lambda}.
##
## The arguments can be of common size or scalar.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: CDF of the exponential distribution

function cdf = exponential_cdf (x, l)

  if (nargin != 2)
    usage ("exponential_cdf (x, lambda)");
  endif

  [retval, x, l] = common_size (x, l);
  if (retval > 0)
    error ("exponential_cdf: x and lambda must be of common size or scalar");
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  l = reshape (l, 1, s);
  cdf = zeros (1, s);

  k = find (isnan (x) | !(l > 0));
  if (any (k))
    cdf(k) = NaN * ones (1, length (k));
  endif

  k = find ((x == Inf) & (l > 0));
  if (any (k))
    cdf(k) = ones (1, length (k));
  endif

  k = find ((x > 0) & (x < Inf) & (l > 0));
  if (any (k))
    cdf (k) = 1 - exp (- l(k) .* x(k));
  endif

  cdf = reshape (cdf, r, c);

endfunction
