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

## usage:  cauchy_pdf (x [, lambda, sigma])
##
## For each element of x, compute the probability density function (PDF)
## at x of the Cauchy distribution with location parameter lambda and
## scale parameter sigma > 0. Default values are lambda = 0, sigma = 1.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  PDF of the Cauchy distribution

function pdf = cauchy_pdf (x, location, scale)
  
  if !(nargin == 1 || nargin == 3)
    usage ("cauchy_pdf (x [, lambda, sigma])");
  endif
  
  if (nargin == 1)
    location = 0;
    scale = 1;
  endif
  
  [retval, x, location, scale] = common_size (x, location, scale);
  if (retval > 0)
    error (["cauchy_pdf:  ", ...
            "x, lambda and sigma must be of common size or scalar"]);
  endif
  
  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  location = reshape (location, 1, s);
  scale = reshape (scale, 1, s);
  
  pdf = NaN * ones (1, s);
  
  k = find ((x > -Inf) & (x < Inf) & (location > -Inf) & 
	    (location < Inf) & (scale > 0) & (scale < Inf));
  if any (k)
    pdf(k) = (1 ./ (1 + ((x(k) - location(k)) ./ scale(k)) .^ 2)) ...
             / pi ./ scale(k);
  endif
  
  pdf = reshape (pdf, r, c);
  
endfunction
