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

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{p}, @var{yf}] =} polyfit (@var{x}, @var{y}, @var{n})
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
## If two output arguments are requested, the second contains the values of
## the polynomial for each value of @var{x}.
## @end deftypefn

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

  y_is_row_vector = (rows (y) == 1);

  l = length (x);
  x = reshape (x, l, 1);
  y = reshape (y, l, 1);

  X = (x * ones (1, n+1)) .^ (ones (l, 1) * (0 : n));

  p = X \ y;

  if (nargout == 2)
    yf = X * p;

    if (y_is_row_vector)
      yf = yf.';
    endif
  endif

  p = flipud (p);

  if (! prefer_column_vectors)
    p = p.';
  endif

endfunction
