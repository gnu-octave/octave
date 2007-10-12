## Copyright (C) 1996, 1998, 2000, 2002, 2004, 2005, 2007
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
## @deftypefn {Function File} {[@var{xinf}, @var{x_ha_err}] =} hinfsyn_ric (@var{a}, @var{bb}, @var{c1}, @var{d1dot}, @var{r}, @var{ptol})
## Forms
## @example
## xx = ([bb; -c1'*d1dot]/r) * [d1dot'*c1 bb'];
## Ha = [a 0*a; -c1'*c1 - a'] - xx;
## @end example
## and solves associated Riccati equation.
## The error code @var{x_ha_err} indicates one of the following
## conditions:
## @table @asis
## @item 0
## successful
## @item 1
## @var{xinf} has imaginary eigenvalues
## @item 2
## @var{hx} not Hamiltonian
## @item 3
## @var{xinf} has infinite eigenvalues (numerical overflow)
## @item 4
## @var{xinf} not symmetric
## @item 5
## @var{xinf} not positive definite
## @item 6
## @var{r} is singular
## @end table
## @end deftypefn

function [Xinf, x_ha_err] = hinfsyn_ric (A, BB, C1, d1dot, R, ptol)

  x_ha_err = 0;        # assume success
  Xinf = [];                 # default return value
  n = issquare(A);
  nw = issquare(R);
  if(rank(R) != nw)    x_ha_err = 6;
  else                 # build hamiltonian Ha for X_inf
    xx = ([BB; -C1'*d1dot]/R) * [d1dot'*C1, BB'];
    Ha = [A, 0*A; -C1'*C1, -A'] - xx;
    x_ha_err = 0;
    [d, Ha] = balance(Ha);
    [u, s] = schur(Ha, "A");
    rev = real(eig(s));

    if (any(abs(rev) <= ptol))  # eigenvalues near the imaginary axis
      x_ha_err = 1;
    elseif (sum(rev > 0) != sum(rev < 0))
      ## unequal number of positive and negative eigenvalues
      x_ha_err = 2;
    else
      ## compute positive Riccati equation solution
      u = d * u;
      Xinf = u(n+1:2*n,1:n) / u(1:n,1:n);
      if (!all(all(finite(Xinf))))
        x_ha_err = 3;
      elseif (norm(Xinf-Xinf') >= 10*ptol)
        ## solution not symmetric
        x_ha_err = 4;
      else
        ## positive semidefinite?
        ## force symmetry (faster, avoids some convergence problems)
        Xinf = (Xinf + Xinf')/2;
        rev = eig(Xinf);
        if (any(rev <= -ptol))
          x_ha_err = 5;
        endif
      endif
    endif
  endif
endfunction
