function [k, p, e] = lqr (a, b, q, r, zz)

# Usage: [k, p, e] = lqr (A, B, Q, R {,Z})
#
# Linear quadratic regulator design for the continuous time system
#
#   dx/dt = A x + B u
#
# to minimize the cost functional
#
#  J = int_0^\infty{ x' Q x + u' R u } 			Z omitted
#
# or
#
#  J = int_0^\infty{ x' Q x + u' R u +2 x' Z u}		Z included
#
# Returns:
#
#   k = state feedback gain, (A - B K) is stable
#   p = solution of algebraic Riccati equation
#   e = closed loop poles of (A - B K)

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if (nargin != 4 && nargin != 5)
    error ("lqr: illegal number of arguments");
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
  
  if ((n1 = is_square (q)) == 0 || n1 != n)
    error ("lqr: q must be square and conformal with a");
  endif

# Check r.
  if((m1 = is_square(r)) == 0 || m1 != m)
    error ("lqr: r must be square and conformal with column dimension of b");
  endif

# Check if n is there.
  if (nargin == 5)
    [n1, m1] = size (zz);
    if (n1 != n || m1 != m)
      error ("lqr: z must be identically dimensioned with b");
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
    p = are (ao, (b/r)*b', qo);
    k = r\(b'*p + zz');
    e = eig (a - b*k);
  else
    error ("lqr: q (r) must be symmetric positive (semi) definite");
  endif

endfunction
