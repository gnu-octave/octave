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

## usage:  cross (x, y)
##
## Computes the vector cross product of the two 3-dimensional vectors
## x and y.

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 15 October 1994
## Adapted-By: jwe

function z = cross (x, y)
  
  if (nargin != 2)
    usage ("cross (x, y)");
  endif

  if (! (is_vector (x) && length (x) == 3
	 && is_vector (y) && length (y) == 3))
    error ("cross: both x and y must be 3-dimensional vectors");
  endif
  
  x = reshape (x, 3, 1);
  y = reshape (y, 3, 1);
  e = eye (3, 3);
  for k = 1 : 3
    z(k) = det ([x y e(:, k)]);
  endfor

endfunction
