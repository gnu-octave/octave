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

  if (length (x) == 3 && length (y) == 3)

    z = [x(2)*y(3) - x(3)*y(2); x(3)*y(1) - x(1)*y(3); x(1)*y(2) - x(2)*y(1)];

    x_nr = rows (x);
    y_nr = rows (y);

    if ((x_nr == y_nr && x_nr == 1)
 	|| (x_nr != y_nr && ! prefer_column_vectors))
      z = z.';
    endif

  else
    error ("cross: both x and y must be 3-dimensional vectors");
  endif

endfunction
