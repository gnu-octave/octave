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
## @deftypefn {Function File} {} gampdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, return the probability density function
## (PDF) at @var{x} of the Gamma distribution with parameters @var{a}
## and @var{b}.
## @end deftypefn
##
## @seealso{gamma, gammaln, gammainc, gamcdf, gaminv, gamrnd}

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: PDF of the Gamma distribution

function pdf = gampdf (x, a, b)

  if (nargin != 3)
    usage ("gampdf (x, a, b)");
  endif

  if (!isscalar (a) || !isscalar(b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("gampdf: x, a and b must be of common size or scalars");
    endif
  endif

  sz = size(x);
  pdf = zeros (sz);

  k = find (!(a > 0) | !(b > 0) | isnan (x));
  if (any (k))
    pdf (k) = NaN;
  endif

  k = find ((x > 0) & (a > 0) & (a <= 1) & (b > 0));
  if (any (k))
    if (isscalar(a) && isscalar(b))
      pdf(k) = ((b .^ a) .* (x(k) .^ (a - 1))
		.* exp(-b .* x(k)) ./ gamma (a));
    else
      pdf(k) = ((b(k) .^ a(k)) .* (x(k) .^ (a(k) - 1))
		.* exp(-b(k) .* x(k)) ./ gamma (a(k)));
    endif
  endif

  k = find ((x > 0) & (a > 1) & (b > 0));
  if (any (k))
    if (isscalar(a) && isscalar(b))
      pdf(k) = exp (a .* log (b) + (a-1) .* log (x(k))
		    - b .* x(k) - gammaln (a));
    else
      pdf(k) = exp (a(k) .* log (b(k)) + (a(k)-1) .* log (x(k))
		    - b(k) .* x(k) - gammaln (a(k)));
    endif
  endif

endfunction
