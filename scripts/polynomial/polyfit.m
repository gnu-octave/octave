## Copyright (C) 1996, 1997 John W. Eaton
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## usage:  [p, yf] = polyfit (x, y, n)
##
## Returns the coefficients of a polynomial p(x) of degree n that
## minimizes sumsq (p(x(i)) - y(i)), i.e., that best fits the data
## in the least squares sense.
##
## If two outputs are requested, also return the values of the
## polynomial for each value of x.

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 13 December 1994
## Adapted-By: jwe

function [p, yf] = polyfit (x, y, n)


  if (nargin != 3)
    usage ("polyfit (x, y, n)");
  endif

  if (! (is_vector (x) && is_vector (y) && size (x) == size (y)))
    error ("polyfit: x and y must be vectors of the same size");
  endif

  if (! (is_scalar (n) && n >= 0 && ! isinf (n) && n == round (n)))
    error ("polyfit: n must be a nonnegative integer");
  endif

  l = length (x);
  x = reshape (x, l, 1);
  y = reshape (y, l, 1);

  ## Unfortunately, the economy QR factorization doesn't really save
  ## memory doing the computation -- the returned values are just
  ## smaller.

  ## [Q, R] = qr (X, 0);
  ## p = flipud (R \ (Q' * y));

  ## XXX FIXME XXX -- this is probably not so good for extreme values of
  ## N or X...

  X = (x * ones (1, n+1)) .^ (ones (l, 1) * (0 : n));

  p = flipud ((X' * X) \ (X' * y));

  if (! prefer_column_vectors)
    p = p';
  endif

  if (nargout == 2)
    yf = X * p;
  endif

endfunction
