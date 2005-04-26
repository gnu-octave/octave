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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

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

function [k, p, e] = dlqr (a, b, q, r, s)

  if (nargin != 4 && nargin != 5)
    error ("dlqr: invalid number of arguments");
  endif

  ## Dimension check is done inside dare.m
  [n,m] = size(b);

  ## Check if s is there.
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

  ## Checking stabilizability and detectability (dimensions are checked inside these calls)
  tol = 200*eps;
  if (is_stabilizable (ao, b,tol,1) == 0)
    error ("dlqr: (a,b) not stabilizable");
  endif
  dflag = is_detectable (ao, qo, tol,1);
  if ( dflag == 0)
    warning ("dlqr: (a,q) not detectable");
  elseif ( dflag == -1)
    error("dlqr: (a,q) has non minimal modes near unit circle");
  end

  ## Compute the Riccati solution
  p = dare (ao, b, qo, r);
  k = (r+b'*p*b)\(b'*p*a + s');
  e = eig (a - b*k);


endfunction

