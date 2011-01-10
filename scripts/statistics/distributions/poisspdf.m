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
## @deftypefn {Function File} {} poisspdf (@var{x}, @var{lambda})
## For each element of @var{x}, compute the probability density function
## (PDF) at @var{x} of the poisson distribution with parameter @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the Poisson distribution

function pdf = poisspdf (x, lambda)

  if (nargin != 2)
    print_usage ();
  endif

  if (!isscalar (lambda))
    [retval, x, lambda] = common_size (x, lambda);
    if (retval > 0)
      error ("poisspdf: X and LAMBDA must be of common size or scalar");
    endif
  endif

  pdf = zeros (size (x));

  k = find (!(lambda > 0) | isnan (x));
  if (any (k))
    pdf(k) = NaN;
  endif

  k = find ((x >= 0) & (x < Inf) & (x == round (x)) & (lambda > 0));
  if (any (k))
    if (isscalar (lambda))
      pdf(k) = exp (x(k) .* log (lambda) - lambda - gammaln (x(k) + 1));
    else
      pdf(k) = exp (x(k) .* log (lambda(k)) - lambda(k) - gammaln (x(k) + 1));
    endif
  endif

endfunction
