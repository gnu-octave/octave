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

## usage:  fv (r, n, p [, l] [, method])
##
## Returns the future value at the end of period n of an investment
## which consisting of n payments of p in each period, assuming an
## interest rate r.
##
## With the optional scalar argument l, one can specify an additional
## lump-sum payment. With the optional argument `method', one can
## specify whether the payments are made at the end ("e", default) or at
## the beginning ("b") of each period.
##
## Note that the rate r is not specified in percent, i.e., one has to
## write 0.05 rather than 5 %.
  
## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Future value of an investment

function v = fv (r, n, p, l, m)
  
  if ((nargin < 3) || (nargin > 5))
    usage ("fv (r, n, p [, l] [, method])");
  endif
  
  if !(is_scalar (r) && (r > -1))
    error ("fv:  r must be a scalar > -1");
  elseif !(is_scalar (n) && (n > 0))
    error ("fv:  n must be a positive scalar");
  elseif !is_scalar (p)
    error ("fv:  p must be a scalar.");
  endif
  
  if (r != 0)
    v = p * ((1 + r)^n - 1) / r;
  else
    v = p * n;
  endif
  
  if (nargin > 3)
    if (nargin == 5)
      if !isstr (m)
        error ("fv:  `method' must be a string");
      endif
    elseif isstr (l)
      m = l;
      l = 0;
    else
      m = "e";
    endif
    if strcmp (m, "b")
      v = v * (1 + r);
    endif
    if is_scalar (l)
      v = v + fvl (r, n, l);
    else
      error ("fv:  l must be a scalar");
    endif
  endif
  
endfunction
      
