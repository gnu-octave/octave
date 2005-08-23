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
## @deftypefn {Function File} {} weibpdf (@var{x}, @var{alpha}, @var{sigma})
## Compute the probability density function (PDF) at @var{x} of the
## Weibull distribution with shape parameter @var{alpha} and scale
## parameter @var{sigma} which is given by
##
## @example
##    alpha * sigma^(-alpha) * x^(alpha-1) * exp(-(x/sigma)^alpha)
## @end example
##
## @noindent
## for @var{x} > 0.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the Weibull distribution

function pdf = weibpdf (x, shape, scale)

  if (nargin != 3)
    usage ("weibpdf (x, alpha, sigma)");
  endif

  if (!isscalar (shape) || !isscalar (scale))
    [retval, x, shape, scale] = common_size (x, shape, scale);
    if (retval > 0)
      error ("weibpdf: x, alpha and sigma must be of common size or scalar");
    endif
  endif

  pdf = NaN * ones (size (x));
  ok = ((shape > 0) & (shape < Inf) & (scale > 0) & (scale < Inf));

  k = find ((x > -Inf) & (x <= 0) & ok);
  if (any (k))
    pdf(k) = 0;
  endif

  k = find ((x > 0) & (x < Inf) & ok);
  if (any (k))
    if (isscalar (shape) && isscalar (scale))
      pdf(k) = (shape .* (scale .^ -shape)
		.* (x(k) .^ (shape - 1))
		.* exp(- (x(k) / scale) .^ shape));
    else
      pdf(k) = (shape(k) .* (scale(k) .^ -shape(k))
		.* (x(k) .^ (shape(k) - 1))
		.* exp(- (x(k) ./ scale(k)) .^ shape(k)));
    endif
  endif

endfunction
