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
## @deftypefn {Function File} {} beta_cdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, returns the CDF at @var{x} of the beta
## distribution with parameters @var{a} and @var{b}, i.e.,
## PROB (beta (@var{a}, @var{b}) <= @var{x}).
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: CDF of the Beta distribution

function cdf = beta_cdf (x, a, b)

  if (nargin != 3)
    usage ("beta_cdf (a, b, x)");
  endif

  [retval, x, a, b] = common_size (x, a, b);
  if (retval > 0)
    error ("beta_cdf: x, a and b must be of common size or scalar");
  endif

  [r, c] = size (x);
  s = r * c;
  x   = reshape (x, s, 1);
  a   = reshape (a, s, 1);
  b   = reshape (b, s, 1);
  cdf = zeros (s, 1);

  k = find (!(a > 0) | !(b > 0) | isnan (x));
  if (any (k))
    cdf (k) = NaN * ones (length (k), 1);
  endif

  k = find ((x >= 1) & (a > 0) & (b > 0));
  if (any (k))
    cdf (k) = ones (length (k), 1);
  endif

  k = find ((x > 0) & (x < 1) & (a > 0) & (b > 0));
  if (any (k))
    cdf (k) = betai (a(k), b(k), x(k));
  endif

  cdf = reshape (cdf, r, c);

endfunction
