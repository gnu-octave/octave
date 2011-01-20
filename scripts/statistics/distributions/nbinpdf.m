## Copyright (C) 1995-2011 Kurt Hornik
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
## @deftypefn {Function File} {} nbinpdf (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the probability density function
## (PDF) at @var{x} of the Pascal (negative binomial) distribution with
## parameters @var{n} and @var{p}.
##
## The number of failures in a Bernoulli experiment with success
## probability @var{p} before the @var{n}-th success follows this
## distribution.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the Pascal (negative binomial) distribution

function pdf = nbinpdf (x, n, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (!isscalar(n) || !isscalar(p))
    [retval, x, n, p] = common_size (x, n, p);
    if (retval > 0)
      error ("nbinpdf: X, N and P must be of common size or scalar");
    endif
  endif

  pdf = zeros (size (x));

  k = find (isnan (x) | (n < 1) | (n == Inf) | (n != round (n))
            | (p < 0) | (p > 1));
  if (any (k))
    pdf(k) = NaN;
  endif

  ## Just for the fun of it ...
  k = find ((x == Inf) & (n > 0) & (n < Inf) & (n == round (n))
            & (p == 0));
  if (any (k))
    pdf(k) = 1;
  endif

  k = find ((x >= 0) & (x < Inf) & (x == round (x)) & (n > 0)
            & (n < Inf) & (n == round (n)) & (p > 0) & (p <= 1));
  if (any (k))
    if (isscalar (n) && isscalar (p))
      pdf(k) = bincoeff (-n, x(k)) .* (p ^ n) .* ((p - 1) .^ x(k));
    else
      pdf(k) = bincoeff (-n(k), x(k)) .* (p(k) .^ n(k)) .* ((p(k) - 1) .^ x(k));
    endif
  endif

endfunction
