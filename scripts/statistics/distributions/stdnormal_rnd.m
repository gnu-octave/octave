## Copyright (C) 1995, 1996, 1997  Kurt Hornik
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

## usage:  stdnormal_rnd (r, c)
##
## Return an r by c matrix of random numbers from the standard normal
## distribution.
  
## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Random deviates from the standard normal distribution

function rnd = stdnormal_rnd (r, c)
  
  if (nargin != 2)
    usage ("stdnormal_rnd (r, c)");
  endif

  if ( !(is_scalar (r) && (r > 0) && (r == round (r))) )
    error ("stdnormal_rnd:  r must be a positive integer");
  endif
  if ( !(is_scalar (c) && (c > 0) && (c == round (c))) )
    error ("stdnormal_rnd:  c must be a positive integer");
  endif

  rnd = randn (r, c);
  
endfunction
