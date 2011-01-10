## Copyright (C) 1995, 1996, 1997, 2005, 2006, 2007, 2008, 2009 Kurt Hornik
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
## @deftypefn {Function File} {} tinv (@var{x}, @var{n})
## For each probability value @var{x}, compute the inverse of the
## cumulative distribution function (CDF) of the t (Student)
## distribution with degrees of freedom @var{n}.  This function is
## analogous to looking in a table for the t-value of a single-tailed
## distribution.
## @end deftypefn

## For very large n, the "correct" formula does not really work well,
## and the quantiles of the standard normal distribution are used
## directly.

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the t distribution

function inv = tinv (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (!isscalar (n))
    [retval, x, n] = common_size (x, n);
    if (retval > 0)
      error ("tinv: X and N must be of common size or scalar");
    endif
  endif

  inv = zeros (size (x));

  k = find ((x < 0) | (x > 1) | isnan (x) | !(n > 0));
  if (any (k))
    inv(k) = NaN;
  endif

  k = find ((x == 0) & (n > 0));
  if (any (k))
    inv(k) = -Inf;
  endif

  k = find ((x == 1) & (n > 0));
  if (any (k))
    inv(k) = Inf;
  endif

  k = find ((x > 0) & (x < 1) & (n > 0) & (n < 10000));
  if (any (k))
    if (isscalar (n))
      inv(k) = (sign (x(k) - 1/2)
                .* sqrt (n .* (1 ./ betainv (2*min (x(k), 1 - x(k)),
                                                 n/2, 1/2) - 1)));
    else
      inv(k) = (sign (x(k) - 1/2)
                .* sqrt (n(k) .* (1 ./ betainv (2*min (x(k), 1 - x(k)),
                                                 n(k)/2, 1/2) - 1)));
    endif
  endif

  ## For large n, use the quantiles of the standard normal
  k = find ((x > 0) & (x < 1) & (n >= 10000));
  if (any (k))
    inv(k) = stdnormal_inv (x(k));
  endif

endfunction
