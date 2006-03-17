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
## @deftypefn {Function File} {} logncdf (@var{x}, @var{mu}, @var{sigma})
## For each element of @var{x}, compute the cumulative distribution
## function (CDF) at @var{x} of the lognormal distribution with
## parameters @var{mu} and @var{sigma}.  If a random variable follows this
## distribution, its logarithm is normally distributed with mean
## @var{mu} and standard deviation @var{sigma}.
##
## Default values are @var{mu} = 1, @var{sigma} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the log normal distribution

function cdf = logncdf (x, mu, sigma)

  if (! ((nargin == 1) || (nargin == 3)))
    usage ("logncdf (x, mu, sigma)");
  endif

  if (nargin == 1)
    mu = 0;
    sigma = 1;
  endif

  ## The following "straightforward" implementation unfortunately does
  ## not work (because exp (Inf) -> NaN etc):
  ## cdf = normal_cdf (log (x), log (mu), sigma);
  ## Hence ...

  if (!isscalar (mu) || !isscalar (sigma))
    [retval, x, mu, sigma] = common_size (x, mu, sigma);
    if (retval > 0)
      error ("logncdf: x, mu and sigma must be of common size or scalars");
    endif
  endif

  cdf = zeros (size (x));

  k = find (isnan (x) | !(sigma > 0) | !(sigma < Inf));
  if (any (k))
    cdf(k) = NaN;
  endif

  k = find ((x == Inf) & (sigma > 0) & (sigma < Inf));
  if (any (k))
    cdf(k) = 1;
  endif

  k = find ((x > 0) & (x < Inf) & (sigma > 0) & (sigma < Inf));
  if (any (k))
    if (isscalar (mu) && isscalar (sigma))
      cdf(k) = stdnormal_cdf ((log (x(k)) - mu) / sigma);
    else
      cdf(k) = stdnormal_cdf ((log (x(k)) - mu(k)) ./ sigma(k));
    endif
  endif

endfunction
