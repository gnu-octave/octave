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
	fprintf (stderr, "dare: opt has an illegal value -- setting to B");
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
      fprintf (stderr, "warning: dare: a,b are not conformable");
    endif

    if (is_controllable (a, b) == 0)
      fprintf ("warning: dare: a,b are not controllable");
    endif

# Check a,c compatibility.

    if (is_observable (a, c) == 0)
      fprintf (stderr "warning: dare: a,c are not observable");
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
      fprintf("warning: dare: r is not square");
    elseif (m1 != m)
      fprintf(stderr, "warning: b,r are not conformable");
    endif

    brb = (b/r)*b';
    atc = a'\c;
    [d, sy] = balance ([a + brb*atc, -brb/(a'); -atc, inv (a')], opt);
    [u, s] = schur(sy,'D');
    u = d*u;
    n1 = n+1;
    n2 = 2*n;
    x = u (n1:n2, 1:n)/u(1:n, 1:n);
  else
    error ("usage: x = dare (a, b, c, r {,opt})");
  endif

endfunction
