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
## @deftypefn {Function File} {} geocdf (@var{x}, @var{p})
## For each element of @var{x}, compute the CDF at @var{x} of the
## geometric distribution with parameter @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the geometric distribution

function cdf = geocdf (x, p)

  if (nargin != 2)
    print_usage ();
  endif

  if (!isscalar (x) && !isscalar (p))
    [retval, x, p] = common_size (x, p);
    if (retval > 0)
      error ("geocdf: X and P must be of common size or scalar");
    endif
  endif

  cdf = zeros (size (x));

  k = find (isnan (x) | !(p >= 0) | !(p <= 1));
  if (any (k))
    cdf(k) = NaN;
  endif

  k = find ((x == Inf) & (p >= 0) & (p <= 1));
  if (any (k))
    cdf(k) = 1;
  endif

  k = find ((x >= 0) & (x < Inf) & (x == round (x)) & (p > 0) & (p <= 1));
  if (any (k))
    if (isscalar (x))
      cdf(k) = 1 - ((1 - p(k)) .^ (x + 1));
    elseif (isscalar (p))
      cdf(k) = 1 - ((1 - p) .^ (x(k) + 1));
    else
      cdf(k) = 1 - ((1 - p(k)) .^ (x(k) + 1));
    endif
  endif

endfunction
