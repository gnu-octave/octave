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

## usage:  H = hurst (x)
##
## Estimates the Hurst parameter of sample x via the rescaled range
## statistic. If x is a matrix, the parameter is estimated for every
## single column.

## Author:  FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description:  Estimate the Hurst parameter

function H = hurst (x)

  if (nargin != 1)
    usage ("hurst (x)");
  endif

  if (is_scalar (x))
    error ("hurst:  x must not be a scalar")
  elseif is_vector (x)
    x = reshape (x, length (x), 1);
  end

  [xr, xc] = size (x);

  s = std (x);
  w = cumsum (x - mean (x));
  RS = (max(w) - min(w)) ./ s;
  H = log (RS) / log (xr);

endfunction
