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

## usage:  pmt (r, n, a [, l] [, method])
##
## Compute the amount of periodic payment necessary to amortize a loan
## of amount a with interest rate r in n periods.
##
## With the optional scalar argument l, one can specify an initial
## lump-sum payment. With the optional string argument `method', one can
## specify whether payments are made at the end ("e", default) or at the
## beginning ("b") of each period.
##
## See also:  pv, nper, rate
  
## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Amount of periodic payment needed to amortize a loan

function p = pmt (r, n, a, l, m)
  
  if ((nargin < 3) || (nargin > 5))
    usage ("pmt (r, n, a [, l] [, method])");
  endif
  
  if !(is_scalar (r) && (r > -1))
    error ("pmt:  rate must be a scalar > -1");
  elseif !(is_scalar (n) && (n > 0))
    error ("pmt:  n must be a positive scalar");
  elseif !(is_scalar (a) && (a > 0))
    error ("pmt:  a must be a positive scalar.");
  endif
  
  if (nargin == 5)
    if !isstr (m)
      error ("pmt:  `method' must be a string");
    endif
  elseif (nargin == 4)
    if isstr (l)
      m = l;
      l = 0;
    else
      m = "e";
    endif
  else
    l = 0;
    m = "e";
  endif
  
  p = r * (a - l * (1 + r)^(-n)) / (1 - (1 + r)^(-n));
  
  if strcmp (m, "b")
    p = p / (1 + r);
  endif
  
  
endfunction
      



