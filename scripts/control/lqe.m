function [k, p, e] = lqe (a, g, c, sigw, sigv, zz)

# Usage: [k, p, e] = lqe (A, G, C, SigW, SigV {,Z})
#
# Linear quadratic estimator (Kalman filter) design for the 
# continuous time system
#
#   dx/dt = A x + B u + G w
#       y = C x + D u + w
#
# where w, v are zero-mean gaussian noise processes with respective
# intensities SigW = cov (w, w) and SigV = cov (v, v).
#
# Z (if specified) is cov(w,v); otherwise cov(w,v) = 0.
#
# Observer structure is dz/dt = A z + B u + k( y - C z - D u).
#
# Returns:
#
#   k = observer gain, (A - K C) is stable
#   p = solution of algebraic Riccati equation
#   e = closed loop poles of (A - K C)

# Written by A. S. Hodel (scotte@eng.auburn.edu) August, 1993.

  if (nargin != 5 && nargin != 6)
    error ("lqe: illegal number of arguments");
  endif

# The problem is dual to the regulator design, so transform to lqr
# call.

  if (nargin == 5)
    [k, p, e] = lqr (a', c', g*sigw*g', sigv);
  else
    [k, p, e] = lqr (a', c', g*sigw*g', sigv, g*zz);
  endif

endfunction
