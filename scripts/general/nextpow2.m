## Copyright (C) 1995, 1996 Kurt Hornik
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

## usage:  nextpow2 (x)
##
## If x is a scalar, returns the first integer n such that
## 2^n >= abs (x). 
##
## If x is a vector, return nextpow2 (length (x)). 

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 7 October 1994
## Adapted-By: jwe

function n = nextpow2 (x)
  
  if (nargin != 1)
    usage ("nextpow2 (x)");
  endif

  if (is_vector (x))
    x = length (x);
  elseif (! is_scalar (x))
    error ("nextpow2: x must be a scalar or a vector");
  endif
  
  [f, n] = log2 (abs (x));
  if (f == 0.5)
    n = n - 1;
  endif
  
endfunction
