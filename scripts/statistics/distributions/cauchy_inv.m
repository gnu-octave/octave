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
## @deftypefn {Function File} {} cauchy_inv (@var{x}, @var{lambda}, @var{sigma})
## For each element of @var{x}, compute the quantile (the inverse of the
## CDF) at @var{x} of the Cauchy distribution with location parameter
## @var{lambda} and scale parameter @var{sigma}.  Default values are
## @var{lambda} = 0, @var{sigma} = 1. 
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Quantile function of the Cauchy distribution

function inv = cauchy_inv (x, location, scale)

  if (! (nargin == 1 || nargin == 3))
    usage ("cauchy_inv (x, lambda, sigma)");
  endif

  if (nargin == 1)
    location = 0;
    scale = 1;
  endif

  if (!isscalar (location) || !isscalar (scale)) 
    [retval, x, location, scale] = common_size (x, location, scale);
    if (retval > 0)
      error ("cauchy_inv: x, lambda and sigma must be of common size or scalar");
    endif
  endif

  sz = size (x);
  inv = NaN * ones (sz);

  ok = ((location > -Inf) & (location < Inf) &
       (scale > 0) & (scale < Inf));

  k = find ((x == 0) & ok);
  if (any (k))
    inv(k) = -Inf;
  endif

  k = find ((x > 0) & (x < 1) & ok);
  if (any (k))
    if (isscalar (location) && isscalar (scale)) 
      inv(k) = location - scale .* cot (pi * x(k));
    else
      inv(k) = location(k) - scale(k) .* cot (pi * x(k));
    endif
  endif

  k = find ((x == 1) & ok);
  if (any (k))
    inv(k) = Inf;
  endif

endfunction
