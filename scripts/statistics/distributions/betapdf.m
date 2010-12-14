## Copyright (C) 1995, 1996, 1997, 2005, 2006, 2007 Kurt Hornik
## Copyright (C) 2010 Christos Dimitrakakis
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
## @deftypefn {Function File} {} betapdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, returns the PDF at @var{x} of the beta
## distribution with parameters @var{a} and @var{b}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>, CD <christos.dimitrakakis@gmail.com>
## Description: PDF of the Beta distribution

function pdf = betapdf (x, a, b)

  if (nargin != 3)
    print_usage ();
  endif
  
  if (!isscalar (a) || !isscalar(b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("betapdf: x, a and b must be of common size or scalar");
    endif
  endif

  sz = size (x);
  pdf = zeros (sz);

  k = find (!(a > 0) | !(b > 0) | isnan (x));
  if (any (k))
    pdf (k) = NaN;
  endif

  k = find ((x > 0) & (x < 1) & (a > 0) & (b > 0) & ((a != 1) | (b != 1)));
  if (any (k))
    if (isscalar(a) && isscalar(b))
      pdf(k) = exp ((a - 1) .* log (x(k))
		            + (b - 1) .* log (1 - x(k))
                    + lgamma(a + b) - lgamma(a) - lgamma(b));
    else
      pdf(k) = exp ((a(k) - 1) .* log (x(k))
		            + (b(k) - 1) .* log (1 - x(k))
                    + lgamma(a(k) + b(k)) - lgamma(a(k)) - lgamma(b(k)));
    endif
  endif

  ## Most important special cases when the density is finite.
  k = find ((x == 0) & (a == 1) & (b > 0) & (b != 1));
  if (any (k))
    if (isscalar(a) && isscalar(b))
      pdf(k) = exp(lgamma(a + b) - lgamma(a) - lgamma(b));
    else
      pdf(k) = exp(lgamma(a(k) + b(k)) - lgamma(a(k)) - lgamma(b(k)));
    endif
  endif

  k = find ((x == 1) & (b == 1) & (a > 0) & (a != 1));
  if (any (k))
    if (isscalar(a) && isscalar(b))
      pdf(k) = exp(lgamma(a + b) - lgamma(a) - lgamma(b));
    else
      pdf(k) = exp(lgamma(a(k) + b(k)) - lgamma(a(k)) - lgamma(b(k)));
    endif
  endif

  k = find ((x >= 0) & (x <= 1) & (a == 1) & (b == 1));
  if (any (k))
    pdf(k) = 1;
  endif

  ## Other special case when the density at the boundary is infinite.
  k = find ((x == 0) & (a < 1));
  if (any (k))
    pdf(k) = Inf;
  endif

  k = find ((x == 1) & (b < 1));
  if (any (k))
    pdf(k) = Inf;
  endif

endfunction

%% Test large values for betapdf
%!assert (betapdf(0.5, 1000, 1000), 35.678, 1e-3)
