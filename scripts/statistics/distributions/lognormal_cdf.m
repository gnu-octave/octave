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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} lognormal_cdf (@var{x}, @var{a}, @var{v})
## For each element of @var{x}, compute the cumulative distribution
## function (CDF) at @var{x} of the lognormal distribution with
## parameters @var{a} and @var{v}.  If a random variable follows this
## distribution, its logarithm is normally distributed with mean
## @code{log (@var{a})} and variance @var{v}.
##
## Default values are @var{a} = 1, @var{v} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: CDF of the log normal distribution

function cdf = lognormal_cdf (x, a, v)

  if (! ((nargin == 1) || (nargin == 3)))
    usage ("lognormal_cdf (x, a, v)");
  endif

  if (nargin == 1)
    a = 1;
    v = 1;
  endif

  ## The following "straightforward" implementation unfortunately does
  ## not work (because exp (Inf) -> NaN etc):
  ## cdf = normal_cdf (log (x), log (a), v);
  ## Hence ...

  [retval, x, a, v] = common_size (x, a, v);
  if (retval > 0)
    error ("lognormal_cdf: x, a and v must be of common size or scalars");
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  a = reshape (a, 1, s);
  v = reshape (v, 1, s);
  cdf = zeros (1, s);

  k = find (isnan (x) | !(a > 0) | !(a < Inf) | !(v > 0) | !(v < Inf));
  if (any (k))
    cdf(k) = NaN * ones (1, length (k));
  endif

  k = find ((x == Inf) & (a > 0) & (a < Inf) & (v > 0) & (v < Inf));
  if (any (k))
    cdf(k) = ones (1, length (k));
  endif

  k = find ((x > 0) & (x < Inf) & (a > 0) & (a < Inf) & (v > 0) & (v < Inf));
  if (any (k))
    cdf(k) = stdnormal_cdf ((log (x(k)) - log (a(k))) ./ sqrt (v(k)));
  endif

  cdf = reshape (cdf, r, c);

endfunction
