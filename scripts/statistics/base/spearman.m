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

## usage:  spearman (x [, y])
##
## Computes Spearman's rank correlation coefficient rho for each of the
## variables specified by the input arguments.
##
## For matrices, each row is an observation and each column a variable;
## vectors are always observations and may be row or column vectors.
##
## spearman (x) is equivalent to spearman (x, x).
##
## For two data vectors x and y, Spearman's rho is the correlation of
## the ranks of x and y.
##
## If x and y are drawn from independent distributions, rho has zero
## mean and variance 1 / (n - 1), and is asymptotically normally
## distributed.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Spearman's rank correlation rho

function rho = spearman (x, y)
  
  if ((nargin < 1) || (nargin > 2))
    usage ("spearman (x [, y])");
  endif

  if (rows (x) == 1)
    x = x';
  endif
  n = rows (x);
  
  if (nargin == 1)
    rho = cor (ranks (x));
  else
    if (rows (y) == 1)
      y = y';
    endif
    if (rows (y) != n)
      error (["spearman:  ", ...
	      "x and y must have the same number of observations"]);
    endif
    rho = cor (ranks (x), ranks (y));
  endif
  
endfunction
