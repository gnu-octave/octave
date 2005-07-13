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
## @deftypefn {Function File} {} expinv (@var{x}, @var{lambda})
## For each element of @var{x}, compute the quantile (the inverse of the
## CDF) at @var{x} of the exponential distribution with parameter
## @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Quantile function of the exponential distribution

function inv = expinv (x, l)

  if (nargin != 2)
    usage ("expinv (x, lambda)");
  endif

  if (!isscalar (x) && !isscalar(l))
    [retval, x, l] = common_size (x, l);
    if (retval > 0)
      error ("expinv: x and lambda must be of common size or scalar");
    endif
  endif

  if (isscalar (x))
    sz = size (l);
  else
    sz = size (x);
  endif

  inv = zeros (sz);

  k = find (!(l > 0) | (x < 0) | (x > 1) | isnan (x));
  if (any (k))
    inv(k) = NaN;
  endif

  k = find ((x == 1) & (l > 0));
  if (any (k))
    inv(k) = Inf;
  endif

  k = find ((x > 0) & (x < 1) & (l > 0));
  if (any (k))
    if isscalar (l)
      inv(k) = - log (1 - x(k)) ./ l;
    elseif isscalar (x)
      inv(k) = - log (1 - x) ./ l(k);
    else
      inv(k) = - log (1 - x(k)) ./ l(k);
    endif
  endif

endfunction

