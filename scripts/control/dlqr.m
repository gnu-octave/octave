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
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function [k, p, e] = dlqr (a, b, q, r, zz)

# Usage: [k, p, e] = dlqr (A, B, Q, R {,Z})
#
# Linear quadratic regulator design for the discrete time system
#
#   x[k+1] = A x[k] + B u[k]
#
# to minimize the cost functional
#
#  J = Sum { x' Q x + u' R u } 			Z omitted
#
# or
#
#  J = Sum { x' Q x + u' R u +2 x' Z u}		Z included
#
# Returns:
#
#   k = state feedback gain, (A - B K) is stable
#   p = solution of algebraic Riccati equation
#   e = closed loop poles of (A - B K)

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.
# Converted to discrete time by R. B. Tenison
# (btenison@eng.auburn.edu) October 1993

  if (nargin != 4 && nargin != 5)
    error ("dlqr: invalid number of arguments");
  endif

# Check a.
  if ((n = is_square (a)) == 0)
    error ("dlqr: requires 1st parameter(a) to be square");
  endif

# Check b.
  [n1, m] = size (b);
  if (n1 != n)
    error ("dlqr: a,b not conformal");
  endif

# Check q.
  
  if ((n1 = is_square (q)) == 0 || n1 != n)
    error ("dlqr: q must be square and conformal with a");
  endif

# Check r.
  if((m1 = is_square(r)) == 0 || m1 != m)
    error ("dlqr: r must be square and conformal with column dimension of b");
  endif

# Check if n is there.
  if (nargin == 5)
    [n1, m1] = size (zz);
    if (n1 != n || m1 != m)
      error ("dlqr: z must be identically dimensioned with b");
    endif

# Incorporate cross term into a and q.

    ao = a - (b/r)*zz';
    qo = q - (zz/r)*zz';
  else
    zz = zeros (n, m);
    ao = a;
    qo = q;
  endif

# Check that q, (r) are symmetric, positive (semi)definite

  if (is_symmetric (q) && is_symmetric (r) ...
      && all (eig (q) >= 0) && all (eig (r) > 0))
    p = dare (ao, b, qo, r);
    k = (r+b'*p*b)\b'*p*a + r\zz';
    e = eig (a - b*k);
  else
    error ("dlqr: q (r) must be symmetric positive (semi) definite");
  endif

endfunction
