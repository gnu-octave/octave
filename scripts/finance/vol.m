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

## usage:  vol (X, m [, n])
##
## vol returns the volatility of each column of the input matrix X. m is
## the number of data sets per period (e.g. the number of data per year
## if you want to compute the volatility per year). The optional
## parameter n gives the number of past periods used for computation, if
## n is omitted, n=1 is used. If T is the number of rows of X, vol
## returns the volatility from n*m to T.

## Author:  FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description:  Volatility of financial time series data

function retval = vol (X, m, n)

  if (nargin < 2)
    usage ("vol (X, m [, n])");
  endif

  [xr, xc] = size (X);

  if (nargin > 2)
    if (n * m > xr)
      error ("vol:  I need more data!");
    endif
  else
    n = 1;
    if (n * m > xr)
      error ("vol:  I need more data!");
    endif
  endif

  U = zeros (xr - 1, xc);

  if all (X)
    U = X ((2 : xr), :) ./ X((1 : (xr-1)), :);
  else
    error ("vol:  zero element in X");
  endif

  U = log(U);
  U = U - ones (xr - 1, 1) * sum (U) / (xr - 1);

  retval = zeros (xr - n * m, xc);

  retval(1, :) = sumsq (U((1 : n*m), :));
  for i = 2 : (xr - n * m)
    retval(i, :) = retval(i - 1, :) ...
	- U(i - 1, :).^2 + U(i + n * m - 1, :).^2;
  endfor

  retval = sqrt (retval * m / (n * m - 1));

endfunction

