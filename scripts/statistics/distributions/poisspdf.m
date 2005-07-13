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
## @deftypefn {Function File} {} poisson_pdf (@var{x}, @var{lambda})
## For each element of @var{x}, compute the probability density function
## (PDF) at @var{x} of the poisson distribution with parameter @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: PDF of the Poisson distribution

function pdf = poisson_pdf (x, l)

  if (nargin != 2)
    usage ("poisson_pdf (x, lambda)");
  endif

  if (!isscalar (l))
    [retval, x, l] = common_size (x, l);
    if (retval > 0)
      error ("poisson_pdf: x and lambda must be of common size or scalar");
    endif
  endif

  pdf = zeros (size (x));

  k = find (!(l > 0) | isnan (x));
  if (any (k))
    pdf(k) = NaN;
  endif

  k = find ((x >= 0) & (x < Inf) & (x == round (x)) & (l > 0));
  if (any (k))
    if (isscalar (l))
      pdf(k) = exp (x(k) .* log (l) - l - gammaln (x(k) + 1));
    else
      pdf(k) = exp (x(k) .* log (l(k)) - l(k) - gammaln (x(k) + 1));
    endif
  endif

endfunction
