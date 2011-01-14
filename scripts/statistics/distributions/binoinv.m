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
## @deftypefn {Function File} {} binoinv (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the quantile at @var{x} of the
## binomial distribution with parameters @var{n} and @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the binomial distribution

function inv = binoinv (x, n, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (!isscalar (n) || !isscalar (p))
    [retval, x, n, p] = common_size (x, n, p);
    if (retval > 0)
      error ("binoinv: X, N and P must be of common size or scalars");
    endif
  endif
  
  sz = size (x);
  inv = zeros (sz);

  k = find (!(x >= 0) | !(x <= 1) | !(n >= 0) | (n != round (n))
            | !(p >= 0) | !(p <= 1));
  if (any (k))
    inv(k) = NaN;
  endif

  k = find ((x >= 0) & (x <= 1) & (n >= 0) & (n == round (n))
            & (p >= 0) & (p <= 1));
  if (any (k))
    if (isscalar (n) && isscalar (p))
      cdf = binopdf (0, n, p) * ones (size(k));
      while (any (inv(k) < n))
        m = find (cdf < x(k));
        if (any (m))
          inv(k(m)) = inv(k(m)) + 1;
          cdf(m) = cdf(m) + binopdf (inv(k(m)), n, p);
        else
          break;
        endif
      endwhile
    else 
      cdf = binopdf (0, n(k), p(k));
      while (any (inv(k) < n(k)))
        m = find (cdf < x(k));
        if (any (m))
          inv(k(m)) = inv(k(m)) + 1;
          cdf(m) = cdf(m) + binopdf (inv(k(m)), n(k(m)), p(k(m)));
        else
          break;
        endif
      endwhile
    endif
  endif

endfunction
