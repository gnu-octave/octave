function gramian = dgram (A, B)

#  Usage: gramian = dgram (A, B)
#
#  Returns the discrete controllability and observability gramian.
#
#  dgram (A, B)   returns the discrete controllability gramian.
#  dgram (A', C') returns the observability gramian.

#  Written by R. Bruce Tenison (btenison@eng.auburn.edu)
#  October 1993

  [U, Sig, V] = svd (B);

  gramian = U * dlyap (U'*A*U, Sig*Sig') * U';

endfunction
