## Copyright (C) 1995, 1996, 1997  Friedrich Leisch
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

## usage:  [d, D] = diffpara (X [, a [, b]])
##
## Returns the estimator d for the differencing parameter of an
## integrated time series.
##
## The frequencies from [2*pi*a/T, 2*pi*b/T] are used for the
## estimation. If b is omitted, the interval [2*pi/T, 2*pi*a/T] is used,
## if both b and a are omitted then a = 0.5 * sqrt(T) and b = 1.5 *
## sqrt(T) is used, where T is the sample size. If X is a matrix, the
## differencing parameter of every single column is estimated.
##
## D contains the estimators for all frequencies in the intervals
## described above, d is simply mean(D).
##
## Reference: Brockwell, Peter J. & Davis, Richard A. Time Series:
## Theory and Methods Springer 1987

## Author:  FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description:  Estimate the fractional differencing parameter

function [d, D] = diffpara (X, a, b)

  if ((nargin < 1) || (nargin > 3))
    usage ("[d [, D]] = diffpara (X [, a [, b]])");
  else
    if is_vector (X)
      n = length (X);
      k = 1;
      X = reshape (X, n, 1);
    else
      [n, k] = size(X);
    endif
    if (nargin == 1)
      a = 0.5 * sqrt (n);
      b = 1.5 * sqrt (n);
    elseif (nargin == 2)
      b = a;
      a = 1;
    endif
  endif

  if !(is_scalar (a) && is_scalar (b))
    error ("diffpara:  a and b must be scalars");
  endif

  D = zeros (b - a + 1, k);

  for l = 1:k

    w = 2 * pi * (1 : n-1) / n;

    x = 2 * log (abs( 1 - exp (-i*w)));
    y = log (periodogram (X(2:n,l)));

    x = center (x);
    y = center (y);

    for m = a:b
      D(m-a+1) = - x(1:m) * y(1:m) / sumsq (x(1:m));
    endfor

  endfor

  d = mean (D);

endfunction

