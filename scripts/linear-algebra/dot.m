## Copyright (C) 1998 John W. Eaton
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

## usage:  dot (x, y)
##
## Computes the dot product of two vectors.

## Author: jwe

function z = dot (x, y)
  
  if (nargin != 2)
    usage ("dot (x, y)");
  endif

  if (is_vector (x) && is_vector (y) && length (x) == length (y))
    [x_nr, x_nc] = size (x);
    [y_nr, y_nc] = size (y);
    if (x_nr == 1)
      if (y_nr == 1)
	z = x * y';
      else
	z = x * y;
      endif
    else
      if (y_nr == 1)
	z = y * x;
      else
	z = y' * x;
      endif
    endif
  else
    error ("dot: both arguments must be vectors of the same length");
  endif

endfunction
