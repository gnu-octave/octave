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

## Calculates likelihood for the ordinal logistic regression model.
## Called by logistic_regression.
  
## Author:  Gordon K. Smyth <gks@maths.uq.oz.au>
## Adapted-By:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Likelihood in logistic regression

function [g, g1, p, dev] ...
      = logistic_regression_likelihood (y, x, beta, z, z1)
  
  e = exp ([z x] * beta); e1 = exp ([z1 x] * beta);
  g = e ./ (1 + e); g1 = e1 ./ (1 + e1);
  g = max (y == max (y), g); g1 = min (y > min(y), g1);
  
  p = g - g1;
  dev = -2 * sum (log (p));  

endfunction
