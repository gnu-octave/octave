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
## @deftypefn {Function File} {} fcdf (@var{x}, @var{m}, @var{n})
## For each element of @var{x}, compute the CDF at @var{x} of the F
## distribution with @var{m} and @var{n} degrees of freedom, i.e.,
## PROB (F (@var{m}, @var{n}) @leq{} @var{x}). 
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the F distribution

function cdf = fcdf (x, m, n)

  if (nargin != 3)
    print_usage ();
  endif

  if (!isscalar (m) || !isscalar (n))
    [retval, x, m, n] = common_size (x, m, n);
    if (retval > 0)
      error ("fcdf: x, m and n must be of common size or scalar");
    endif
  endif

  sz = size (x);
  cdf = zeros (sz);

  k = find (!(m > 0) | !(n > 0) | isnan (x));
  if (any (k))
    cdf(k) = NaN;
  endif

  k = find ((x == Inf) & (m > 0) & (n > 0));
  if (any (k))
    cdf(k) = 1;
  endif

  k = find ((x > 0) & (x < Inf) & (m > 0) & (n > 0));
  if (any (k))
    if (isscalar (m) && isscalar (n))
      cdf(k) = 1 - betainc (1 ./ (1 + m .* x(k) ./ n), n / 2, m / 2);
    else
      cdf(k) = 1 - betainc (1 ./ (1 + m(k) .* x(k) ./ n(k)), n(k) / 2, 
                            m(k) / 2);
    endif
  endif

endfunction
