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

## usage:  nper (r, p, a [, l] [, method])
##
## Computes the number of regular payments of p necessary to amortize a
## loan of amount a and interest r.
##
## With the optional scalar argument l, one can specify an additional
## lump-sum payment of l made at the end of the amortization time. With
## the optional string argument `method', one can specify whether
## payments are made at the end ("e", default) or at the beginning ("b")
## of each period.
##
## Note that the rate r is not specified in percent, i.e., one has to
## write 0.05 rather than 5 %.
##
## See also:  pv, pmt, rate;  npv.
  
## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Number of payments needed for amortizing a loan

function n = nper (r, p, a, l, m)
  
  if ((nargin < 3) || (nargin > 5))
    usage ("nper (r, p, a [, l] [, method])");
  endif
  
  if !(is_scalar (r) && (r > -1))
    error ("nper:  r must be a scalar > -1");
  elseif !is_scalar (p)
    error ("nper:  p must be a scalar");
  elseif !is_scalar (a)
    error ("nper:  a must be a scalar");
  endif

  if (nargin == 5)
    if !isstr (m)
      error ("nper:  `method' must be a string");
    endif
  elseif (nargin == 4)
    if isstr (l)
      m = l;
      l = 0;
    else
      m = "e";
    endif
  else
    m = "e";
    l = 0;
  endif
  
  if strcmp (m, "b")
    p = p * (1 + r);
  endif
  
  q = (p - r * a) / (p - r * l);
  
  if (q > 0)
    n = - log (q) / log (1 + r);
  else
    n = Inf;
  endif
  
endfunction

