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

## usage:  mean (x [, opt])
##
## For vector arguments, return the mean the values.
## For matrix arguments, return a row vector containing the mean for
## each column.
##
## With the optional argument opt, the kind of mean computed can be
## selected.
## If opt is "a", the (ordinary) arithmetic mean is computed.  This
## is the default.
## If opt is "g", the geometric mean is computed.
## If opt is "h", the harmonic mean is computed.
  
## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Compute arithmetic, geometric, and harmonic mean

function y = mean (x, opt)

  if ((nargin < 1) || (nargin > 2))
    usage ("mean (x [, opt])");
  endif

  if isempty (x)
    error ("mean:  x must not be empty");
  endif
  
  if (rows (x) == 1)
    x = x';
  endif
  
  if (nargin == 1)
    opt = "a";
  endif

  [r, c] = size (x);
  
  if (strcmp (opt, "a"))
    y = sum (x) / r;
  elseif (strcmp (opt, "g"))
    y = NaN * ones (1, c);
    i = find (all (x > 0));
    if any (i)
      y(i) = exp (sum (log (x(:, i))) / r);
    endif
  elseif (strcmp (opt, "h"))
    y = NaN * ones (1, c);
    i = find (all (x != 0));
    if any (i)
      y(i) = r ./ sum (1 ./ x(:, i));
    endif
  else
    error (sprintf ("mean:  option `%s' not recognized", opt));
  endif
    
endfunction
