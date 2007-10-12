## Copyright (C) 1993, 1994, 1995 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} dlyap (@var{a}, @var{b})
## Solve the discrete-time Lyapunov equation
##
## @strong{Inputs}
## @table @var
##   @item a
##   @var{n} by @var{n} matrix;
##   @item b
##   Matrix: @var{n} by @var{n}, @var{n} by @var{m}, or @var{p} by @var{n}.
## @end table
##
## @strong{Output}
## @table @var
## @item x
## matrix satisfying appropriate discrete time Lyapunov equation.
## @end table
##
## Options:
## @itemize @bullet
## @item @var{b} is square: solve 
## @iftex
## @tex
## $$ axa^T - x + b = 0 $$
## @end tex
## @end iftex
## @ifinfo
## @code{a x a' - x + b = 0}
## @end ifinfo
## @item @var{b} is not square: @var{x} satisfies either
## @iftex
## @tex
## $$ axa^T - x + bb^T = 0 $$
## @end tex
## @end iftex
## @ifinfo
## @example
## a x a' - x + b b' = 0
## @end example
## @end ifinfo
## @noindent
## or
## @iftex
## @tex
## $$ a^Txa - x + b^Tb = 0, $$
## @end tex
## @end iftex
## @ifinfo
## @example
## a' x a - x + b' b = 0,
## @end example
## @end ifinfo
## @noindent
## whichever is appropriate.
## @end itemize
##
## @strong{Method}
## Uses Schur decomposition method as in Kitagawa,
## @cite{An Algorithm for Solving the Matrix Equation @math{X = F X F' + S}},
## International Journal of Control, Volume 25, Number 5, pages 745--753
## (1977).
##
## Column-by-column solution method as suggested in
## Hammarling, @cite{Numerical Solution of the Stable, Non-Negative
## Definite Lyapunov Equation}, @acronym{IMA} Journal of Numerical Analysis, Volume
## 2, pages 303--323 (1982).
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1993

function x = dlyap (a, b)

  if ((n = issquare (a)) == 0)
    warning ("dlyap: a must be square");
  endif

  if ((m = issquare (b)) == 0)
    [n1, m] = size (b);
    if (n1 == n)
      b = b*b';
      m = n1;
    else
      b = b'*b;
      a = a';
    endif
  endif

  if (n != m)
    warning ("dlyap: a,b not conformably dimensioned");
  endif

  ## Solve the equation column by column.

  [u, s] = schur (a);
  b = u'*b*u;

  j = n;
  while (j > 0)
    j1 = j;

    ## Check for Schur block.

    if (j == 1)
      blksiz = 1;
    elseif (s (j, j-1) != 0)
      blksiz = 2;
      j = j - 1;
    else
      blksiz = 1;
    endif

    Ajj = kron (s (j:j1, j:j1), s) - eye (blksiz*n);

    rhs = reshape (b (:, j:j1), blksiz*n, 1);

    if (j1 < n)
      rhs2 = s*(x (:, (j1+1):n) * s (j:j1, (j1+1):n)');
      rhs = rhs + reshape (rhs2, blksiz*n, 1);
    endif

    v = - Ajj\rhs;
    x (:, j) = v (1:n);

    if(blksiz == 2)
      x (:, j1) = v ((n+1):blksiz*n);
    endif

    j = j - 1;

  endwhile

  ## Back-transform to original coordinates.

  x = u*x*u';

endfunction
