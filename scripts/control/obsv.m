# Copyright (C) 1997 Kai P. Mueller
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

function Qb = obsv(sys, c)
  # ------------------------------------------------------
  # Qb = obsv(sys [, c])
  # Build observability matrix
  #
  #          +          +
  #          | C        |
  #          | CA       |
  #     Qb = | CA^2     |
  #          | ...      |
  #          | CA^(n-1) |
  #          +          +
  #
  # of a system data structure or the pair (A, C).
  #
  # Note: obsv() forms the observability matrix.
  #       The numerical properties of is_observable()
  #       are much better for observability tests.
  # See also:  ctrb, is_observable, is_controllable
  # ------------------------------------------------------

  # Written by Kai P. Mueller November 4, 1997
  # modified by

  if (nargin == 2)
    a = sys;
  elseif (nargin == 1 && is_struct(sys))
    sysupdate(sys,"ss");
    [a,b,c] = sys2ss(sys);
  else
    usage("obsv(sys [, c])")
  endif

  if (!is_abcd(a,c'))
    Qb = [];
  else
    # no need to check dimensions, we trust is_abcd().
    [na, ma] = size(a);
    [nc, mc] = size(c);
    Qb = zeros(na*nc, ma);
    for i = 1:na
      Qb((i-1)*nc+1:i*nc, :) = c;
      c = c * a;
    endfor
  endif
endfunction
