## Copyright (C) 1995, 1996, 1997, 2005, 2006, 2007 Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} gampdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, return the probability density function
## (PDF) at @var{x} of the Gamma distribution with parameters @var{a}
## and @var{b}.
## @seealso{gamma, gammaln, gammainc, gamcdf, gaminv, gamrnd}
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: PDF of the Gamma distribution

function pdf = gampdf (x, a, b)

  if (nargin != 3)
    print_usage ();
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
      pdf(k) = (x(k) .^ (a - 1)) ...
                .* exp(- x(k) ./ b) ./ gamma (a) ./ (b .^ a);
    else
      pdf(k) = (x(k) .^ (a(k) - 1)) ...
                .* exp(- x(k) ./ b(k)) ./ gamma (a(k)) ./ (b(k) .^ a(k));
    endif
  endif

  k = find ((x > 0) & (a > 1) & (b > 0));
  if (any (k))
    if (isscalar(a) && isscalar(b))
      pdf(k) = exp (- a .* log (b) + (a-1) .* log (x(k))
                    - x(k) ./ b - gammaln (a));
    else
      pdf(k) = exp (- a(k) .* log (b(k)) + (a(k)-1) .* log (x(k))
                    - x(k) ./ b(k) - gammaln (a(k)));
    endif
  endif

endfunction
