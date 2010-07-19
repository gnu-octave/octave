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
## @deftypefn {Function File} {} betacdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, returns the CDF at @var{x} of the beta
## distribution with parameters @var{a} and @var{b}, i.e.,
## PROB (beta (@var{a}, @var{b}) @leq{} @var{x}).
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the Beta distribution

function cdf = betacdf (x, a, b)

  if (nargin != 3)
    print_usage ();
  endif

  if (!isscalar (a) || !isscalar(b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("betacdf: x, a and b must be of common size or scalar");
    endif
  endif

  sz = size(x);
  cdf = zeros (sz);

  k = find (!(a > 0) | !(b > 0) | isnan (x));
  if (any (k))
    cdf (k) = NaN;
  endif

  k = find ((x >= 1) & (a > 0) & (b > 0));
  if (any (k))
    cdf (k) = 1;
  endif

  k = find ((x > 0) & (x < 1) & (a > 0) & (b > 0));
  if (any (k))
    if (isscalar (a) && isscalar(b))
      cdf (k) = betainc (x(k), a, b);
    else
      cdf (k) = betainc (x(k), a(k), b(k));
    endif
  endif

endfunction
