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
## @deftypefn {Function File} {} poisson_inv (@var{x}, @var{lambda})
## For each component of @var{x}, compute the quantile (the inverse of
## the CDF) at @var{x} of the Poisson distribution with parameter
## @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Quantile function of the Poisson distribution

function inv = poisson_inv (x, l)

  if (nargin != 2)
    usage ("poisson_inv (x, lambda)");
  endif

  [retval, x, l] = common_size (x, l);
  if (retval > 0)
    error ("poisson_inv: x and lambda must be of common size or scalar");
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  l = reshape (l, 1, s);
  inv = zeros (1, s);

  k = find ((x < 0) | (x > 1) | isnan (x) | !(l > 0));
  if (any (k))
    inv(k) = NaN * ones (1, length (k));
  endif

  k = find ((x == 1) & (l > 0));
  if (any (k))
    inv(k) = Inf * ones (1, length (k));
  endif

  k = find ((x > 0) & (x < 1) & (l > 0));
  if (any (k))
    cdf = exp (-l(k));
    while (1)
      m = find (cdf < x(k));
      if (any (m))
        inv(k(m)) = inv(k(m)) + 1;
        cdf(m) = cdf(m) + poisson_pdf (inv(k(m)), l(k(m)));
      else
        break;
      endif
    endwhile
  endif

  inv = reshape (inv, r, c);

endfunction