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
## @deftypefn {Function File} {} normcdf (@var{x}, @var{m}, @var{s})
## For each element of @var{x}, compute the cumulative distribution
## function (CDF) at @var{x} of the normal distribution with mean
## @var{m} and standard deviation @var{s}.
##
## Default values are @var{m} = 0, @var{s} = 1.
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: CDF of the normal distribution

function cdf = normcdf (x, m, s)

  if (! ((nargin == 1) || (nargin == 3)))
    print_usage ();
  endif

  if (nargin == 1)
    m = 0;
    s = 1;
  endif

  if (!isscalar (m) || !isscalar (s))
    [retval, x, m, s] = common_size (x, m, s);
    if (retval > 0)
      error ("normcdf: X, M and S must be of common size or scalar");
    endif
  endif

  sz = size (x);
  cdf = zeros (sz);

  if (isscalar (m) && isscalar(s))
    if (find (isinf (m) | isnan (m) | !(s >= 0) | !(s < Inf)))
      cdf = NaN (sz);
    else
      cdf =  stdnormal_cdf ((x - m) ./ s);
    endif
  else
    k = find (isinf (m) | isnan (m) | !(s >= 0) | !(s < Inf));
    if (any (k))
      cdf(k) = NaN;
    endif

    k = find (!isinf (m) & !isnan (m) & (s >= 0) & (s < Inf));
    if (any (k))
      cdf(k) = stdnormal_cdf ((x(k) - m(k)) ./ s(k));
    endif
  endif

  cdf((s == 0) & (x == m)) = 0.5;

endfunction
