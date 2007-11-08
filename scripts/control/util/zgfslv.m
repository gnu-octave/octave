## Copyright (C) 1996, 1998, 2000, 2005, 2007
##               Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} zgfslv (@var{n}, @var{m}, @var{p}, @var{b})
## Solve system of equations for dense zgep problem.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Converted to Octave by R Bruce Tenison, July 3, 1994

function x = zgfslv (n, m, p, b)

  if (nargin != 4)
    print_usage ();
  endif

  nmp = n+m+p;
  gam1 = (2*n)+m+p;    gam2 = n+p;     gam3 = n+m;

  G1 = givens(sqrt(m),-sqrt(p))';
  G2 = givens(m+p,sqrt(n*(m+p)))';

  x = b;

  ## 1) U1 e^n = sqrt(n)e_1^n
  ## 2) U2 e^m = sqrt(m)e_1^m
  ## 3) U3 e^p = sqrt(p)e_1^p
  xdx1 = 1:n; xdx2 = n+(1:m); xdx3 = n+m+(1:p);
  x(xdx1,1) = zgshsr(x(xdx1,1));
  x(xdx2,1) = zgshsr(x(xdx2,1));
  x(xdx3,1) = zgshsr(x(xdx3,1));

  ## 4) Givens rotations to reduce stray non-zero elements
  idx1 = [n+1,n+m+1];     idx2 = [1,n+1];
  x(idx1) = G1'*x(idx1);
  x(idx2) = G2'*x(idx2);

  ## 6) Scale x, then back-transform to get x
  en = ones(n,1);  em = ones(m,1);   ep = ones(p,1);
  lam = [gam1*en;gam2*em;gam3*ep];
  lam(1) = n+m+p;
  lam(n+1) = 1;       # dummy value to avoid divide by zero
  lam(n+m+1)=n+m+p;

  x = x ./ lam;       x(n+1) = 0;  # minimum norm solution

  ## back transform now.
  x(idx2) = G2*x(idx2);
  x(idx1) = G1*x(idx1);
  x(xdx3,1) = zgshsr(x(xdx3,1));
  x(xdx2,1) = zgshsr(x(xdx2,1));
  x(xdx1,1) = zgshsr(x(xdx1,1));

endfunction

