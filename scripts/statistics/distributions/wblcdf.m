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
## @deftypefn {Function File} {} wblcdf (@var{x}, @var{scale}, @var{shape})
## Compute the cumulative distribution function (CDF) at @var{x} of the
## Weibull distribution with shape parameter @var{scale} and scale
## parameter @var{shape}, which is
##
## @example
## 1 - exp(-(x/shape)^scale)
## @end example
##
## @noindent
## for @var{x} >= 0.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the Weibull distribution

function cdf = wblcdf (x, scale, shape)

  if (nargin < 1 || nargin > 3)
    usage ("wblcdf (x, scale, shape)");
  endif

  if (nargin < 3)
    shape = 1;
  endif

  if (nargin < 2)
    scale = 1;
  endif

  if (!isscalar (shape) || !isscalar (scale))
    [retval, x, shape, scale] = common_size (x, shape, scale);
    if (retval > 0)
      error ("wblcdf: x, scale and shape must be of common size or scalar");
    endif
  endif

  cdf = NaN * ones (size (x));

  ok = ((shape > 0) & (shape < Inf) & (scale > 0) & (scale < Inf));

  k = find ((x <= 0) & ok);
  if (any (k))
    cdf(k) = 0;
  endif

  k = find ((x > 0) & (x < Inf) & ok);
  if (any (k))
    if (isscalar (shape) && isscalar (scale))
      cdf(k) = 1 - exp (- (x(k) / scale) .^ shape);
    else
      cdf(k) = 1 - exp (- (x(k) ./ scale(k)) .^ shape(k));
    endif
  endif

  k = find ((x == Inf) & ok);
  if (any (k))
    cdf(k) = 1;
  endif

endfunction
