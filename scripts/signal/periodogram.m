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

## usage:  periodogram (x)
##
## For a data matrix x from a sample of size n, return the periodogram.
  
## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description:  Compute the periodogram

function retval = periodogram (x)

  [r c] = size(x);

  if (r == 1)
    r = c;
  endif
  
  retval = (abs (fft (x - mean (x)))) .^ 2 / r;
  
endfunction

  





