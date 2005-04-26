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
## @deftypefn {Function File} {} pascal_inv (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the quantile at @var{x} of the
## Pascal (negative binomial) distribution with parameters @var{n} and
## @var{p}.
##
## The number of failures in a Bernoulli experiment with success
## probability @var{p} before the @var{n}-th success follows this
## distribution.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Quantile function of the Pascal distribution

function inv = pascal_inv (x, n, p)

  if (nargin != 3)
    usage ("pascal_inv (x, n, p)");
  endif

  if (!isscalar(n) || !isscalar(p)) 
    [retval, x, n, p] = common_size (x, n, p);
    if (retval > 0)
      error ("pascal_inv: x, n and p must be of common size or scalar");
    endif
  endif

  inv = zeros (size (x));

  k = find (isnan (x) | (x < 0) | (x > 1) | (n < 1) | (n == Inf)
	    | (n != round (n)) | (p < 0) | (p > 1));
  if (any (k))
    inv(k) = NaN;
  endif

  k = find ((x == 1) & (n > 0) & (n < Inf) & (n == round (n))
	    & (p >= 0) & (p <= 1));
  if (any (k))
    inv(k) = Inf;
  endif

  k = find ((x >= 0) & (x < 1) & (n > 0) & (n < Inf)
	    & (n == round (n)) & (p > 0) & (p <= 1));
  if (any (k))
    m = zeros (size (k));
    x = x(k);
    if (isscalar (n) && isscalar (p))
      s = p ^ n * ones (size(k));
      while (1)
	l = find (s < x);
	if (any (l))
          m(l) = m(l) + 1;
          s(l) = s(l) + pascal_pdf (m(l), n, p);
	else
          break;
	endif
      endwhile
    else
      n = n(k);
      p = p(k);
      s = p .^ n;
      while (1)
	l = find (s < x);
	if (any (l))
          m(l) = m(l) + 1;
          s(l) = s(l) + pascal_pdf (m(l), n(l), p(l));
	else
          break;
	endif
      endwhile
    endif
    inv(k) = m;
  endif

endfunction
