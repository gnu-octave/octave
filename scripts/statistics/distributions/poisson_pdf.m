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

## usage:  poisson_pdf (x, lambda)
##
## For each element of x, compute the probability density function (PDF)
## at x of the poisson distribution with parameter lambda.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  PDF of the Poisson distribution

function pdf = poisson_pdf (x, l)
  
  if (nargin != 2)
    usage ("poisson_pdf (x, lambda)");
  endif
  
  [retval, x, l] = common_size (x, l);
  if (retval > 0)
    error (["poisson_pdf:  ", ...
	    "x and lambda must be of common size or scalar"]);
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  l = reshape (l, 1, s);
  pdf = zeros (1, s);

  k = find (!(l > 0) | isnan (x));
  if any (k)
    pdf(k) = NaN * ones (1, length (k));
  endif

  k = find ((x >= 0) & (x < Inf) & (x == round (x)) & (l > 0));
  if any (k)
    pdf(k) = exp (x(k) .* log (l(k)) - l(k) - lgamma (x(k) + 1));
  endif
  
  pdf = reshape (pdf, r, c);
  
endfunction
