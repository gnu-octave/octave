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

## usage:  weibull_pdf (x, alpha, sigma)
##
## Compute the probability density function (PDF) at x of the Weibull
## distribution with shape parameter alpha and scale parameter sigma
## which is given by
##    alpha * sigma^(-alpha) * x^(alpha-1) * exp(-(x/sigma)^alpha)
## for x > 0.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  PDF of the Weibull distribution

function pdf = weibull_pdf (x, shape, scale)

  if (nargin != 3)
    usage ("weibull_pdf (x, alpha, sigma)");
  endif

  [retval, x, shape, scale] = common_size (x, shape, scale);
  if (retval > 0)
    error (["weibull_pdf:  ", ...
            "x, alpha and sigma must be of common size or scalar"]);
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  shape = reshape (shape, 1, s);
  scale = reshape (scale, 1, s);

  pdf = NaN * ones (1, s);
  ok = ((shape > 0) & (shape < Inf) & (scale > 0) & (scale < Inf));

  k = find ((x > -Inf) & (x <= 0) & ok);
  if any (k)
    pdf(k) = zeros (1, length (k));
  endif

  k = find ((x > 0) & (x < Inf) & ok);
  if any (k)
    pdf(k) = (shape(k) .* (scale(k) .^ shape(k))
              .* (x(k) .^ (shape(k) - 1))
              .* exp(- (x(k) ./ scale(k)) .^ shape(k)));
  endif

  pdf = reshape (pdf, r, c);

endfunction
