## Copyright (C) 1995, 1996, 1997  Kurt Hornik
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

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

  [retval, x, n, p] = common_size (x, n, p);
  if (retval > 0)
    error ("pascal_inv: x, n and p must be of common size or scalar");
  endif

  [r, c] = size (x);
  s = r * c;
  x   = reshape (x, 1, s);
  n   = reshape (n, 1, s);
  p   = reshape (p, 1, s);
  inv = zeros (1, s);

  k = find (isnan (x) | (x < 0) | (x > 1) | (n < 1) | (n == Inf)
	    | (n != round (n)) | (p < 0) | (p > 1));
  if (any (k))
    inv(k) = NaN * ones (1, length (k));
  endif

  k = find ((x == 1) & (n > 0) & (n < Inf) & (n == round (n))
	    & (p >= 0) & (p <= 1));
  if (any (k))
    inv(k) = Inf * ones (1, length (k));
  endif

  k = find ((x >= 0) & (x < 1) & (n > 0) & (n < Inf)
	    & (n == round (n)) & (p > 0) & (p <= 1));
  if (any (k))
    x = x(k);
    n = n(k);
    p = p(k);
    m = zeros (1, length (k));
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
    inv(k) = m;
  endif

  inv = reshape (inv, r, c);

endfunction
