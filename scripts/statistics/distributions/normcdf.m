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
## @deftypefn {Function File} {} normcdf (@var{x}, @var{m}, @var{v})
## For each element of @var{x}, compute the cumulative distribution
## function (CDF) at @var{x} of the normal distribution with mean
## @var{m} and variance @var{v}.
##
## Default values are @var{m} = 0, @var{v} = 1.
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: CDF of the normal distribution

function cdf = normcdf (x, m, v)

  if (! ((nargin == 1) || (nargin == 3)))
    print_usage ();
  endif

  if (nargin == 1)
    m = 0;
    v = 1;
  endif

  if (!isscalar (m) || !isscalar(v))
    [retval, x, m, v] = common_size (x, m, v);
    if (retval > 0)
      error ("normcdf: x, m and v must be of common size or scalar");
    endif
  endif

  sz = size (x);
  cdf = zeros (sz);

  if (isscalar (m) && isscalar(v))
    if (find (isinf (m) | isnan (m) | !(v >= 0) | !(v < Inf)))
      cdf = NaN * ones (sz);
    else
      cdf =  stdnormal_cdf ((x - m) ./ sqrt (v));
    endif
  else
    k = find (isinf (m) | isnan (m) | !(v >= 0) | !(v < Inf));
    if (any (k))
      cdf(k) = NaN;
    endif

    k = find (!isinf (m) & !isnan (m) & (v >= 0) & (v < Inf));
    if (any (k))
      cdf(k) = stdnormal_cdf ((x(k) - m(k)) ./ sqrt (v(k)));
    endif
  endif

  cdf((v == 0) & (x == m)) = 0.5;

endfunction
