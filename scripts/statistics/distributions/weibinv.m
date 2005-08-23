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
## @deftypefn {Function File} {} weibinv (@var{x}, @var{lambda}, @var{alpha})
## Compute the quantile (the inverse of the CDF) at @var{x} of the
## Weibull distribution with shape parameter @var{alpha} and scale
## parameter @var{sigma}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the Weibull distribution

function inv = weibinv (x, shape, scale)

  if (nargin != 3)
    usage ("weibinv (x, alpha, sigma)");
  endif

  if (!isscalar (shape) || !isscalar (scale))
    [retval, x, shape, scale] = common_size (x, shape, scale);
    if (retval > 0)
      error ("weibinv: x, alpha and sigma must be of common size or scalar");
    endif
  endif

  inv = NaN * ones (size (x));

  ok = ((shape > 0) & (shape < Inf) & (scale > 0) & (scale < Inf));

  k = find ((x == 0) & ok);
  if (any (k))
    inv(k) = -Inf;
  endif

  k = find ((x > 0) & (x < 1) & ok);
  if (any (k))
    if (isscalar (shape) && isscalar (scale))
      inv(k) = scale * (- log (1 - x(k))) .^ (1 / shape);
    else
      inv(k) = scale(k) .* (- log (1 - x(k))) .^ (1 ./ shape(k));
    endif
  endif

  k = find ((x == 1) & ok);
  if (any (k))
    inv(k) = Inf;
  endif

endfunction
