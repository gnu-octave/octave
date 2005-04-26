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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{p}, @var{s}] =} polyfit (@var{x}, @var{y}, @var{n})
## Return the coefficients of a polynomial @var{p}(@var{x}) of degree
## @var{n} that minimizes
## @iftex
## @tex
## $$
## \sum_{i=1}^N (p(x_i) - y_i)^2
## $$
## @end tex
## @end iftex
## @ifinfo
## @code{sumsq (p(x(i)) - y(i))},
## @end ifinfo
##  to best fit the data in the least squares sense.
##
## The polynomial coefficients are returned in a row vector.
##
## If two output arguments are requested, the second is a structure
## containing the following fields:
##
## @table @code
## @item R
## The Cholesky factor of the Vandermonde matrix used to compute the
## polynomial coefficients.
## @item X
## The Vandermonde matrix used to compute the polynomial coefficients.
## @item df
## The degrees of freedom.
## @item normr
## The norm of the residuals.
## @item yf
## The values of the polynomial for each value of @var{x}.
## @end table
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 13 December 1994
## Adapted-By: jwe

function [p, s, mu] = polyfit (x, y, n)


  if (nargin != 3)
    usage ("polyfit (x, y, n)");
  endif

  if (! (isvector (x) && isvector (y) && size (x) == size (y)))
    error ("polyfit: x and y must be vectors of the same size");
  endif

  if (! (isscalar (n) && n >= 0 && ! isinf (n) && n == round (n)))
    error ("polyfit: n must be a nonnegative integer");
  endif

  y_is_row_vector = (rows (y) == 1);

  l = length (x);
  x = reshape (x, l, 1);
  y = reshape (y, l, 1);

  X = (x * ones (1, n+1)) .^ (ones (l, 1) * (n : -1 : 0));

  p = X \ y;

  if (nargout > 1)

    yf = X*p;

    if (y_is_row_vector)
      s.yf = yf.';
    else
      s.yf = yf;
    endif

    [s.R, dummy] = chol (X'*X);
    s.X = X;
    s.df = l - n - 1;
    s.normr = norm (yf - y);

  endif

endfunction
