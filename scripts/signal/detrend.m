## Copyright (C) 1995, 1996  Kurt Hornik
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

## usage:  detrend (x [, p])
##
## If x is a vector, detrend (x, p) removes the best fit of a
## polynomial of order p from the data x.
##
## If x is a matrix, detrend (x, p) does the same for each column.
##
## If p is not specified, p = 1 is used, i.e., a linear trend is
## removed.

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 11 October 1994
## Adapted-By: jwe
  
function y = detrend (x, p)
  
  if (nargin == 1)
    p = 1;
  elseif (nargin == 2)
    if (! (is_scalar (p) && p == round (p) && p >= 0))
      error ("detrend:  p must be a nonnegative integer");
    endif
  else
    usage ("detrend (x [, p])");
  endif
  
  [m, n] = size (x);
  if (m == 1)
    x = x';
  endif
  
  r = rows (x);
  b = ((1 : r)' * ones (1, p + 1)) .^ (ones (r, 1) * (0 : p));
  y = x - b * (b \ x);
  
  if (m == 1)
    y = y';
  endif
  
endfunction
