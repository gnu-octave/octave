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

## -*- texinfo -*-
## @deftypefn {Function File} {} commutation_matrix (@var{m}, @var{n})
## Return the commutation matrix
## @iftex
## @tex
##  $K_{m,n}$
## @end tex
## @end iftex
## @ifinfo
##  K(m,n)
## @end ifinfo
##  which is the unique
## @iftex
## @tex
##  $m n \times m n$
## @end tex
## @end iftex
## @ifinfo
##  @var{m}*@var{n} by @var{m}*@var{n}
## @end ifinfo
##  matrix such that
## @iftex
## @tex
##  $K_{m,n} \cdot {\rm vec} (A) = {\rm vec} (A^T)$
## @end tex
## @end iftex
## @ifinfo
##  @var{K}(@var{m},@var{n}) * vec (@var{A}) = vec (@var{A}')
## @end ifinfo
##  for all
## @iftex
## @tex
##  $m\times n$
## @end tex
## @end iftex
## @ifinfo
##  @var{m} by @var{n}
## @end ifinfo
##  matrices
## @iftex
## @tex
##  $A$.
## @end tex
## @end iftex
## @ifinfo
##  @var{A}.
## @end ifinfo
##
## If only one argument @var{m} is given,
## @iftex
## @tex
##  $K_{m,m}$
## @end tex
## @end iftex
## @ifinfo
##  K(m,m)
## @end ifinfo
##  is returned.
##
## See Magnus and Neudecker (1988), Matrix differential calculus with
## applications in statistics and econometrics.
## @end deftypefn

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
