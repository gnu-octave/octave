## Copyright (C) 1993, 1994, 1995 Auburn University
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{k}, @var{p}, @var{e}] =} dlqr (@var{a}, @var{b}, @var{q}, @var{r}, @var{z})
## Construct the linear quadratic regulator for the discrete time system
## @iftex
## @tex
## $$
##  x_{k+1} = A x_k + B u_k
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## x[k+1] = A x[k] + B u[k]
## @end example
##
## @end ifinfo
## to minimize the cost functional
## @iftex
## @tex
## $$
##  J = \sum x^T Q x + u^T R u
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## J = Sum (x' Q x + u' R u)
## @end example
## @end ifinfo
##
## @noindent
## @var{z} omitted or
## @iftex
## @tex
## $$
##  J = \sum x^T Q x + u^T R u + 2 x^T Z u
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## J = Sum (x' Q x + u' R u + 2 x' Z u)
## @end example
##
## @end ifinfo
## @var{z} included.
##
## The following values are returned:
##
## @table @var
## @item k
## The state feedback gain,
## @iftex
## @tex
## $(A - B K)$
## @end tex
## @end iftex
## @ifinfo
## (@var{a} - @var{b}@var{k})
## @end ifinfo
## is stable.
##
## @item p
## The solution of algebraic Riccati equation.
##
## @item e
## The closed loop poles of
## @iftex
## @tex
## $(A - B K)$.
## @end tex
## @end iftex
## @ifinfo
## (@var{a} - @var{b}@var{k}).
## @end ifinfo
## @end table
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1993
## Converted to discrete time by R. B. Tenison
## (btenison@eng.auburn.edu) October 1993
## Modified by Gabriele Pannocchia <pannocchia@ing.unipi.it>
## July 2000

function [k, p, e] = dlqr (a, b, q, r, s)

  if (nargin != 4 && nargin != 5)
    error ("dlqr: invalid number of arguments");
  endif

  ## Check a.
  if ((n = is_square (a)) == 0)
    error ("dlqr: requires 1st parameter(a) to be square");
  endif

  ## Check b.
  [n1, m] = size (b);
  if (n1 != n)
    error ("dlqr: a,b not conformal");
  endif

  ## Check q.
  if ((n1 = is_square (q)) == 0 || n1 != n)
    error ("dlqr: q must be square and conformal with a");
  endif

  ## Check r.
  if((m1 = is_square(r)) == 0 || m1 != m)
    error ("dlqr: r must be square and conformal with column dimension of b");
  endif

  ## Check if n is there.
  if (nargin == 5)
    [n1, m1] = size (s);
    if (n1 != n || m1 != m)
      error ("dlqr: z must be identically dimensioned with b");
    endif

    ## Incorporate cross term into a and q.

    ao = a - (b/r)*s';
    qo = q - (s/r)*s';
  else
    s = zeros (n, m);
    ao = a;
    qo = q;
  endif

  ## Check that q, (r) are symmetric, positive (semi)definite
  if (is_symmetric (q) && is_symmetric (r)
      && all (eig (q) >= 0) && all (eig (r) > 0))
    p = dare (ao, b, qo, r);
    k = (r+b'*p*b)\(b'*p*a + s');
    e = eig (a - b*k);
  else
    error ("dlqr: q (r) must be symmetric positive (semi) definite");
  endif

endfunction

