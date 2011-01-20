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
## @deftypefn {Function File} {} nbincdf (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the CDF at x of the Pascal
## (negative binomial) distribution with parameters @var{n} and @var{p}.
##
## The number of failures in a Bernoulli experiment with success
## probability @var{p} before the @var{n}-th success follows this
## distribution.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the Pascal (negative binomial) distribution

function cdf = nbincdf (x, n, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (!isscalar(n) || !isscalar(p))
    [retval, x, n, p] = common_size (x, n, p);
    if (retval > 0)
      error ("nbincdf: X, N and P must be of common size or scalar");
    endif
  endif

  cdf = zeros (size (x));

  k = find (isnan (x) | (n < 1) | (n == Inf) | (n != round (n))
            | (p < 0) | (p > 1));
  if (any (k))
    cdf(k) = NaN;
  endif

  k = find ((x == Inf) & (n > 0) & (n < Inf) & (n == round (n))
            & (p >= 0) & (p <= 1));
  if (any (k))
    cdf(k) = 1;
  endif

  k = find ((x >= 0) & (x < Inf) & (x == round (x)) & (n > 0)
            & (n < Inf) & (n == round (n)) & (p > 0) & (p <= 1));
  if (any (k))
    ## Does anyone know a better way to do the summation?
    m = zeros (size (k));
    x = floor (x(k));
    y = cdf(k);
    if (isscalar (n) && isscalar (p))
      while (1)
        l = find (m <= x);
        if (any (l))
          y(l) = y(l) + nbinpdf (m(l), n, p);
          m(l) = m(l) + 1;
        else
          break;
        endif
      endwhile
    else
      n = n(k);
      p = p(k);
      while (1)
        l = find (m <= x);
        if (any (l))
          y(l) = y(l) + nbinpdf (m(l), n(l), p(l));
          m(l) = m(l) + 1;
        else
          break;
        endif
      endwhile
    endif
    cdf(k) = y;
  endif

endfunction
