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
## @deftypefn {Function File} {} f_cdf (@var{x}, @var{m}, @var{n})
## For each element of @var{x}, compute the CDF at @var{x} of the F
## distribution with @var{m} and @var{n} degrees of freedom, i.e.,
## PROB (F (@var{m}, @var{n}) <= @var{x}). 
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: CDF of the F distribution

function cdf = f_cdf (x, m, n)

  if (nargin != 3)
    usage ("f_cdf (x, m, n)");
  endif

  [retval, x, m, n] = common_size (x, m, n);
  if (retval > 0)
    error ("f_cdf: x, m and n must be of common size or scalar");
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  m = reshape (m, 1, s);
  n = reshape (n, 1, s);
  cdf = zeros (1, s);

  k = find (!(m > 0) | !(n > 0) | isnan (x));
  if (any (k))
    cdf(k) = NaN * ones (1, length (k));
  endif

  k = find ((x == Inf) & (m > 0) & (n > 0));
  if (any (k))
    cdf(k) = ones (1, length (k));
  endif

  k = find ((x > 0) & (x < Inf) & (m > 0) & (n > 0));
  if (any (k))
    cdf(k) = 1 - betai (n(k) / 2, m(k) / 2, 1 ./ (1 + m(k) .* x(k) ./ n(k)));
  endif

  cdf = reshape (cdf, r, c);

endfunction
