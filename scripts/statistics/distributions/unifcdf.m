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
## @deftypefn {Function File} {} unifcdf (@var{x}, @var{a}, @var{b})
## Return the CDF at @var{x} of the uniform distribution on [@var{a},
## @var{b}], i.e., PROB (uniform (@var{a}, @var{b}) @leq{} x).
##
## Default values are @var{a} = 0, @var{b} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the uniform distribution

function cdf = unifcdf (x, a, b)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (nargin == 1)
    a = 0;
    b = 1;
  endif

  if (!isscalar (a) || !isscalar(b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("unifcdf: X, A and B must be of common size or scalar");
    endif
  endif

  sz = size (x);
  cdf = zeros (sz);

  k = find (isnan (x) | !(a < b));
  if (any (k))
    cdf(k) = NaN;
  endif

  k = find ((x >= b) & (a < b));
  if (any (k))
    cdf(k) = 1;
  endif
  
  k = find ((x > a) & (x < b));
  if (any (k))
    if (isscalar (a) && isscalar(b))
      cdf(k) = (x(k) < b) .* (x(k) - a) ./ (b - a);
    else
      cdf(k) = (x(k) < b(k)) .* (x(k) - a(k)) ./ (b(k) - a(k));
    endif
  endif

endfunction
