## Copyright (C) 1996 John W. Eaton
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

## usage: gcd (a, ...)
##
## [g [, v]] = gcd (a) returns the greatest common divisor g of the
## entries of the integer vector a, and an integer vector v such that
## g = v(1) * a(k) + ... + v(k) * a(k).
##
## [g [, v]] = gcd (a1, ..., ak) is the same with a = [a1, ..., ak].

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 16 September 1994
## Adapted-By: jwe

function [g, v] = gcd (a, ...)

  if (nargin > 1)
    va_start;
    for k = 2:nargin;
      a = [a, (va_arg ())];
    endfor
  endif

  if (round (a) != a)
    error ("gcd: all arguments must be integer");
  endif

  g = abs (a(1));
  v = sign (a(1));
  for k = 1:(length (a) - 1)
    x = [g, 1, 0];
    y = [(abs (a(k+1))), 0, 1];
    while (y(1) > 0)
      r = x - y * floor (x(1) / y(1));
      x = y;
      y = r;
    endwhile
    g = x(1);
    v = [x(2) * v, x(3) * (sign (a(k+1)))];
  endfor

endfunction
