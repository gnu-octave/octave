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

## usage:  y = log2 (x) or [f, e] = log2 (x)
##
## y = log2 (x) returns the logarithm of base 2 of x.
##
## [f, e] = log2 (x) returns f and e with 1/2 <= abs(f) < 1 and
## x = f * 2^e.

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 17 October 1994
## Adapted-By: jwe

function [f, e] = log2 (x)

  if (nargin != 1)
    usage ("y = log2 (x) or [f, e] = log2 (x)");
  endif

  if (nargout < 2)
    f = log (x) / log (2);
  elseif (nargout == 2)
    ## Only deal with the real parts ...
    x = real (x);
    ## Since log (0) gives problems, 0 entries are replaced by 1.  
    ## This is corrected later by multiplication with the sign.
    f = abs (x) + (x == 0);
    e = (floor (log (f) / log (2)) + 1) .* (x != 0);
    f = sign (x) .* f ./ (2 .^ e);
  else
    error ("log2 takes at most 2 output arguments");
  endif

endfunction

