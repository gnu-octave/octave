## Copyright (C) 1995, 1996  Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} duplication_matrix (@var{n})
## Return the duplication matrix
## @iftex
## @tex
##  $D_n$
## @end tex
## @end iftex
## @ifinfo
##  @math{Dn}
## @end ifinfo
##  which is the unique
## @iftex
## @tex
##  $n^2 \times n(n+1)/2$
## @end tex
## @end iftex
## @ifinfo
##  @math{n^2} by @math{n*(n+1)/2}
## @end ifinfo
##  matrix such that
## @iftex
## @tex
##  $D_n * {\rm vech} (A) = {\rm vec} (A)$
## @end tex
## @end iftex
## @ifinfo
##  @math{Dn vech (A) = vec (A)}
## @end ifinfo
##  for all symmetric
## @iftex
## @tex
##  $n \times n$
## @end tex
## @end iftex
## @ifinfo
##  @math{n} by @math{n}
## @end ifinfo
##  matrices
## @iftex
## @tex
##  $A$.
## @end tex
## @end iftex
## @ifinfo
##  @math{A}.
## @end ifinfo
##
## See Magnus and Neudecker (1988), Matrix differential calculus with
## applications in statistics and econometrics.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 8 May 1995
## Adapged-By: jwe

function d = duplication_matrix (n)

  if (nargin != 1)
    usage ("duplication_matrix (n)");
  endif

  if (! (isscalar (n) && n == round (n) && n > 0))
    error ("duplication_matrix: n must be a positive integer");
  endif

  d = zeros (n * n, n * (n + 1) / 2);

  ## It is clearly possible to make this a LOT faster!
  count = 0;
  for j = 1 : n
    d ((j - 1) * n + j, count + j) = 1;
    for i = (j + 1) : n
      d ((j - 1) * n + i, count + i) = 1;
      d ((i - 1) * n + j, count + i) = 1;
    endfor
    count = count + n - j;
  endfor

endfunction
