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

## usage:  cov (x [, y])
##
## The (i,j)-th entry of cov (x, y) is the covariance between the i-th
## variable in x and the j-th variable in y.
##
## For matrices, each row is an observation and each column a variable;
## vectors are always observations and may be row or column vectors.
##
## cov (x) is cov (x, x).

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Compute covariances

function c = cov (x, y)

  if (nargin < 1 || nargin > 2)
    usage ("cov (x [, y])");
  endif

  if (rows (x) == 1)
    x = x';
  endif
  n = rows (x);

  if (nargin == 2)
    if (rows (y) == 1)
      y = y';
    endif
    if (rows (y) != n)
      error ("cov: x and y must have the same number of observations."); 
    endif
    x = x - ones (n, 1) * sum (x) / n;
    y = y - ones (n, 1) * sum (y) / n;
    c = conj (x' * y / (n - 1));
  elseif (nargin == 1)
    x = x - ones (n, 1) * sum (x) / n;
    c = conj (x' * x / (n - 1));
  endif

endfunction
