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

## -*- texinfo -*- 
## @deftypefn {Function File} {} rate (@var{n}, @var{p}, @var{v} [, @var{l}] [,@var{method}])
## Computes the rate of return on an investment of present value @var{v} which
## pays @var{p} in @var{n} consecutive periods.
##
## With the optional scalar argument @var{l}, one can specify an additional
## lump-sum payment made at the end of @var{n} periods. With the optional
## string argument @var{`method'}, one can specify whether payments are made
## at the end ("e", default) or at the beginning ("b") of each period.
## @end deftypefn
## @seealso{pv, pmt, nper, and npv}
  
## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Rate of return of an investment
  
function r = rate (n, p, v, l, m)

  if ((nargin < 3) || (nargin > 5))
    usage ("rate (n, p, v [, l] [, method])");
  endif
  
  if !(is_scalar (n) && (n > 0))
    error ("rate:  n must be a positive scalar");
  elseif !is_scalar (p)
    error ("rate:  p must be a scalar");
  elseif !is_scalar (v)
    error ("rate:  p must be a scalar");
  endif

  if (nargin == 5)
    if !isstr (m)
      error ("rate:  `method' must be a string");
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
  
  if !is_scalar (l)
    error ("rate:  l must be a scalar");
  endif
  
  string = ["function delta = f (r) ", ...
      "delta = pv (r, %g, %g, %g, \"%s\") - %g;  end"];
  eval (sprintf (string, n, p, l, m, v));
  
  [r, info] = fsolve ("f", 0);
  
endfunction
