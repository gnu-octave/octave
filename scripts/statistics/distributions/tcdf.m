## Copyright (C) 1995, 1996, 1997, 2005, 2006, 2007, 2008 Kurt Hornik
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
## @deftypefn {Function File} {} tcdf (@var{x}, @var{n})
## For each element of @var{x}, compute the cumulative distribution
## function (CDF) at @var{x} of the t (Student) distribution with
## @var{n} degrees of freedom, i.e., PROB (t(@var{n}) @leq{} @var{x}).
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the t distribution

function cdf = tcdf (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (!isscalar (n))
    [retval, x, n] = common_size (x, n);
    if (retval > 0)
      error ("tcdf: x and n must be of common size or scalar");
    endif
  endif

  cdf = zeros (size (x));

  k = find (isnan (x) | !(n > 0));
  if (any (k))
    cdf(k) = NaN;
  endif

  k = find ((x == Inf) & (n > 0));
  if (any (k))
    cdf(k) = 1;
  endif

  k = find ((x > -Inf) & (x < Inf) & (n > 0));
  if (any (k))
    if (isscalar (n))
      cdf(k) = betainc (1 ./ (1 + x(k) .^ 2 ./ n), n / 2, 1 / 2) / 2;
    else
      cdf(k) = betainc (1 ./ (1 + x(k) .^ 2 ./ n(k)), n(k) / 2, 1 / 2) / 2;
    endif
    ind = find (x(k) > 0);
    if (any (ind))
      cdf(k(ind)) = 1 - cdf(k(ind));
    endif
  endif

endfunction
