# Copyright (C) 1993, 1994, 1995 John W. Eaton
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
# Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

function x = dare (a, b, c, r, opt)

# Usage: x = dare (a, b, c, r {,opt})
#
# Solves discrete-time algebraic riccati equation
#
#   a' x a - x + a' x b (r + b' x b)^{-1} b' x a + c = 0
#
# for
#
#   a: nxn
#   b: nxm
#   c: nxn, symmetric positive semidefinite 
#   r: mxm, invertible
#
# If c is not square, then the function attempts to use c'*c instead.
#
# Solution method: Laub's Schur method (IEEE Trans Auto Contr, 1979) applied
# to the appropriate symplectic matrix.
#
# See also: Ran and Rodman, "Stable Hermitian Solutions of Discrete
# Algebraic Riccati Equations," Mathematics of Control, Signals and
# Systems, Vol 5, no 2 (1992)  pp 165-194.
#
# opt is an option passed to the eigenvalue balancing routine default
# is "B". 
#
# See also: balance, are

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if (nargin == 4 || nargin == 5)
    if (nargin == 5)
      if (opt != "N" || opt != "P" || opt != "S" || opt != "B")
	warning ("dare: opt has an invalid value -- setting to B");
	opt = "B";
      endif
    else
      opt = "B";
    endif

# Check a matrix dimensions
    if ((n = is_square (a)) == 0)
      error ("dare: a is not square");
    endif

# Check a,b compatibility.

    [n1, m] = size (b);

    if (n1 != n)
      warning ("dare: a,b are not conformable");
    endif

    if (is_controllable (a, b) == 0)
      warning ("dare: a,b are not controllable");
    endif

# Check a,c compatibility.

    if (is_observable (a, c) == 0)
      warning ("dare: a,c are not observable");
    endif

    if ((p = is_square (c)) == 0)
      c = c'*c;
      p = rows (c);
    endif

    if (n != p)
      error ("dare: a,c are not conformable");
    endif

# Check r dimensions.

    if ((m1 = is_square (r)) == 0)
      warning ("dare: r is not square");
    elseif (m1 != m)
      warning ("b,r are not conformable");
    endif

    brb = (b/r)*b';
    atc = a'\c;
    [d, sy] = balance ([a + brb*atc, -brb/(a'); -atc, (inv (a'))], opt);
    [u, s] = schur (sy, 'D');
    u = d*u;
    n1 = n+1;
    n2 = 2*n;
    x = u (n1:n2, 1:n)/u(1:n, 1:n);
  else
    usage ("x = dare (a, b, c, r {,opt})");
  endif

endfunction
