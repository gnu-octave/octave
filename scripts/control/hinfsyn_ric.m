# Copyright (C) 1996,1998 Auburn University.  All Rights Reserved
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function [Xinf,x_ha_err] = hinfsyn_ric(A,BB,C1,d1dot,R,ptol)
#
# forms
#        xx = ([BB; -C1'*d1dot]/R) * [d1dot'*C1 BB'];
#        Ha = [A 0*A; -C1'*C1 -A'] - xx;
# and solves associated Riccati equation
# returns error code
#  x_ha_err:
#    0: successful
#    1: Xinf has imaginary eigenvalues
#    2: Hx not Hamiltonian
#    3: Xinf has inf. eigenvalues (numerical overflow)
#    4: Xinf not symmetric
#    5: Xinf not positive definite
#    6: R is singular

x_ha_err = 0;        # assume success
Xinf = [];		     # default return value
n = is_square(A);
nw = is_square(R);
if(rank(R) != nw)    x_ha_err = 6;
else                 # build hamiltonian Ha for X_inf
  xx = ([BB; -C1'*d1dot]/R) * [d1dot'*C1, BB'];
  Ha = [A, 0*A; -C1'*C1, -A'] - xx;
  x_ha_err = 0;
  [d, Ha] = balance(Ha);
  [u, s] = schur(Ha, "A");
  rev = real(eig(s));

  if (any(abs(rev) <= ptol))	# eigenvalues near the imaginary axis
    x_ha_err = 1;
  elseif (sum(rev > 0) != sum(rev < 0))
    # unequal number of positive and negative eigenvalues
    x_ha_err = 2;
  else
    # compute positive Riccati equation solution
    u = d * u;
    Xinf = u(n+1:2*n,1:n) / u(1:n,1:n);
    if (!all(all(finite(Xinf))))
      x_ha_err = 3;
    elseif (norm(Xinf-Xinf') >= 10*ptol)
      # solution not symmetric
      x_ha_err = 4;
    else
      # positive semidefinite?
      # force symmetry (faster, avoids some convergence problems)
      Xinf = (Xinf + Xinf')/2;
      rev = eig(Xinf);
      if (any(rev <= -ptol))
        x_ha_err = 5;
      endif
    endif
  endif
endif
