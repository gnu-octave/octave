function A = compan(c)

# compan (c)
# Compute the companion matrix corresponding to polynomial vector c.
#
# In octave a polynomial is represented by it's coefficients (arranged
# in descending order). For example a vector c of length n+1 corresponds
# to the following nth order polynomial
#
#   p(x) = c(1) x^n + ... + c(n) x + c(n+1).
#
# The corresponding companion matrix is
#         _                                                        _
#        |  -c(2)/c(1)   -c(3)/c(1)  ...  -c(n)/c(1)  -c(n+1)/c(1)  |
#        |       1            0      ...       0             0      |
#        |       0            1      ...       0             0      |
#    A = |       .            .   .            .             .      |
#        |       .            .       .        .             .      |
#        |       .            .           .    .             .      |
#        |_      0            0      ...       1             0     _|
#
# The eigenvalues of the companion matrix are equal to the roots of the
# polynomial.
#
# SEE ALSO: poly, roots, residue, conv, deconv, polyval, polyderiv, polyinteg

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  June 1994

  if(nargin != 1)
    usage ("compan(vector)");
  endif

  if(!is_vector(c))
    error("compan: expecting a vector argument.");
  endif

  # Ensure that c is a row vector.
  if(rows(c) > 1)
    c = c.';
  endif

  n = length(c);
  A = diag(ones(n-2,1),-1);
  A(1,:) = -c(2:n)/c(1);

endfunction
