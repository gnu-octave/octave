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

## usage:  kendall (x [, y])
##
## Computes Kendall's tau for each of the variables specified by the
## input arguments.
##
## For matrices, each row is an observation and each column a variable;
## vectors are always observations and may be row or column vectors.
##
## kendall (x) is equivalent to kendall (x, x).
##
## For two data vectors x, y of common length n, Kendall's tau is the
## correlation of the signs of all rank differences of x and y;  i.e.,
## if both x and y have distinct entries, then \tau = \frac{1}{n(n-1)}
## \sum_{i,j} SIGN(q_i-q_j) SIGN(r_i-r_j), where the q_i and r_i are the
## ranks of x and y, respectively.
##
## If x and y are drawn from independent distributions, Kendall's tau is
## asymptotically normal with mean 0 and variance (2 * (2n+5)) / (9 * n
## * (n-1)).

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Kendall's rank correlation tau

function tau = kendall (x, y)
  
  if ((nargin < 1) || (nargin > 2))
    usage ("kendall (x [, y])");
  endif

  if (rows (x) == 1)
    x = x';
  endif
  [n, c] = size (x);

  if (nargin == 2)
    if (rows (y) == 1)
      y = y';
    endif
    if (rows (y) != n)
      error (["kendall:  ", ...
	      "x and y must have the same number of observations"]);
    else
      x = [x, y];
    endif
  endif
  
  r   = ranks (x);
  m   = sign (kron (r, ones (n, 1)) - kron (ones (n, 1), r));
  tau = cor (m);
  
  if (nargin == 2)
    tau = tau (1 : c, (c + 1) : columns (x));
  endif
  
endfunction