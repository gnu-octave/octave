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

## usage:  commutation_matrix (m [, n])
## 
## Returns the commutation matrix K_{m,n} which is the unique m*n by
## m*n matrix such that K_{m,n} * vec (A) = vec (A') for all m by n
## matrices A.
##
## If only one argument m is given, K_{m,m} is returned.
##
## See Magnus and Neudecker (1988), Matrix differential calculus with
## applications in statistics and econometrics.

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 8 May 1995
## Adapted-By: jwe

function k = commutation_matrix (m, n)
  
  if (nargin < 1 || nargin > 2)
    usage ("commutation_matrix (m [, n])");
  else
    if (! (is_scalar (m) && m == round (m) && m > 0))
      error ("commutation_matrix: m must be a positive integer");
    endif
    if (nargin == 1)
      n = m;
    elseif (! (is_scalar (n) && n == round (n) && n > 0))
      error ("commutation_matrix: n must be a positive integer");
    endif
  endif
  
  ## It is clearly possible to make this a LOT faster!
  k = zeros (m * n, m * n);
  for i = 1 : m
    for j = 1 : n
      k ((i - 1) * n + j, (j - 1) * m + i) = 1;
    endfor
  endfor

endfunction
