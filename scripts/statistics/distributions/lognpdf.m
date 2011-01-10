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
## @deftypefn {Function File} {} lognpdf (@var{x}, @var{mu}, @var{sigma})
## For each element of @var{x}, compute the probability density function
## (PDF) at @var{x} of the lognormal distribution with parameters
## @var{mu} and @var{sigma}.  If a random variable follows this distribution,
## its logarithm is normally distributed with mean @var{mu}
## and standard deviation @var{sigma}.
##
## Default values are @var{mu} = 1, @var{sigma} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the log normal distribution

function pdf = lognpdf (x, mu, sigma)

  if (! ((nargin == 1) || (nargin == 3)))
    print_usage ();
  endif

  if (nargin == 1)
    mu = 0;
    sigma = 1;
  endif

  ## The following "straightforward" implementation unfortunately does
  ## not work for the special cases (Inf, ...)
  ## pdf = (x > 0) ./ x .* normpdf (log (x), mu, sigma);
  ## Hence ...

  if (!isscalar (mu) || !isscalar (sigma))
    [retval, x, mu, sigma] = common_size (x, mu, sigma);
    if (retval > 0)
      error ("lognpdf: X, MU and SIGMA must be of common size or scalars");
    endif
  endif

  pdf = zeros (size (x));

  k = find (isnan (x) | !(sigma > 0) | !(sigma < Inf));
  if (any (k))
    pdf(k) = NaN;
  endif

  k = find ((x > 0) & (x < Inf) & (sigma > 0) & (sigma < Inf));
  if (any (k))
    if (isscalar (mu) && isscalar (sigma))
      pdf(k) = normpdf (log (x(k)), mu, sigma) ./ x(k);
    else
      pdf(k) = normpdf (log (x(k)), mu(k), sigma(k)) ./ x(k);
    endif
  endif

endfunction
