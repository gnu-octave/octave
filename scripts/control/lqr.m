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

function [k, p, e] = lqr (a, b, q, r, s)

# Usage: [k, p, e] = lqr (A, B, Q, R {,S})
#
# Linear quadratic regulator design for the continuous time system
#   dx/dt = A x + B u
# to minimize the cost functional
#
#  J = int_0^\infty{ [x' u'] [Q  S'; S R] [x ; u] dt}
#
# inputs:
#   A, B: coefficient matrices for continuous time system
#   Q, R, S: cost functional coefficient matrices.
#      Q: must be nonnegative definite.
#      R: must be positive definite
#      S: defaults to 0
# 
# if S is omitted, the optimization simplifies to the usual
#
#  J = int_0^\infty{ x' Q x + u' R u }
#
# Returns:
#
#   k = state feedback gain, (A - B K) is stable and minimizes the
#       cost functional
#   p = solution of algebraic Riccati equation
#   e = closed loop poles of (A - B K)
#
# reference: Anderson and Moore, OPTIMAL CONTROL: LINEAR QUADRATIC METHODS,
# Prentice-Hall, 1990, pp. 56-58

# $Revision: 1.15 $

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  # disp("lqr: entry");

  if ((nargin != 4) && (nargin != 5))
    error ("lqr: invalid number of arguments");
  endif

# Check a.
  if ((n = is_square (a)) == 0)
    error ("lqr: requires 1st parameter(a) to be square");
  endif

# Check b.
  [n1, m] = size (b);
  if (n1 != n)
    error ("lqr: a,b not conformal");
  endif

# Check q.
  
  if ( ((n1 = is_square (q)) == 0) || (n1 != n))
    error ("lqr: q must be square and conformal with a");
  endif

# Check r.
  if ( ((m1 = is_square(r)) == 0) || (m1 != m))
    error ("lqr: r must be square and conformal with column dimension of b");
  endif

# Check if n is there.
  if (nargin == 5)
    [n1, m1] = size (s);
    if ( (n1 != n) || (m1 != m))
      error ("lqr: z must be identically dimensioned with b");
    endif

# Incorporate cross term into a and q.
    ao = a - (b/r)*s';
    qo = q - (s/r)*s';
  else
    s = zeros (n, m);
    ao = a;
    qo = q;
  endif

# Check that q, (r) are symmetric, positive (semi)definite

  if (is_symmetric (q) && is_symmetric (r) ...
      && all (eig (q) >= 0) && all (eig (r) > 0))
    p = are (ao, (b/r)*b', qo);
    k = r\(b'*p + s');
    e = eig (a - b*k);
  else
    error ("lqr: q (r) must be symmetric positive (semi) definite");
  endif

  # disp("lqr: exit");
endfunction
