function y = polyvalm(c,x)
#Evaluate a polynomial in the matrix sense.
#
#In octave, a polynomial is represented by it's coefficients (arranged
#in descending order). For example a vector c of length n+1 corresponds
#to the following nth order polynomial
#
#  p(x) = c(1) x^n + ... + c(n) x + c(n+1).
#
#polyvalm(c,X) will evaluate the polynomial in the matrix sense, i.e. matrix
#multiplication is used instead of element by element multiplication as is
#used in polyval.
#
#X must be a square matrix.
#
#SEE ALSO: polyval, poly, roots, conv, deconv, residue, filter,
#          polyderiv, polyinteg

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  June 1994

  if(nargin != 2)
    error("usage: polyvalm(c,x)");
  endif

  if(is_matrix(c))
    error("poly: first argument must be a vector.");
  endif

  if(!is_square(x))
    error("poly: second argument must be a square matrix.");
  endif

  [v, d] = eig(x);

  y = v * diag(polyval(c,diag(d))) * v';

endfunction
