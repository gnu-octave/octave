function [Ad, Bd] = c2d (Ac, Bc, T)

# Usage: [Ad, Bd] = c2d (Ac, Bc, T)
#
# converts the continuous time system described by:
#   .
#   x = Ac x + Bc u
#
# into a discrete time equivalent model via the matrix exponential
#
#   x[n+1] = Ad x[n] + Bd u[n]
#
# assuming a zero-order hold on the input and sample time T.

# Written by R.B. Tenison (btenison@eng.auburn.edu)
# October 1993

# check args
  if (nargin != 3)
    error ("usage: c2d (Ac, Bc, T)");
  endif

  [ma, na] = size (Ac);
  [mb, nb] = size (Bc);

  if (ma != na)
    error ("c2d: Ac must be square");
  endif

  if (ma != mb)
    error ("c2d: Ac and Bc must have the same number of rows");
  endif

  matexp = expm ([[Ac, Bc] * T; zeros (nb, na+nb)]);

  Ad = matexp (1:na, 1:na);
  Bd = matexp (1:na, na+1:na+nb);

endfunction
