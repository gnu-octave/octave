function r = roots(c)
#Find the roots of a polynomial.
#
#In octave, a polynomial is represented by it's coefficients (arranged
#in descending order). For example, a vector c of length n+1 corresponds
#to the following nth order polynomial
#
#  p(x) = c(1) x^n + ... + c(n) x + c(n+1).
#
#roots(c) will return a vector r of length n such that
#
#  p(x) = c(1) [ (x - r(1)) * (x - r(2)) * ... * (x - r(n)) ]
#
#roots and poly are inverse functions to within a scaling factor.
#
#SEE ALSO: poly, roots, conv, deconv, residue, filter, polyval, polyvalm,
#          polyderiv, polyinteg

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  June 1994

  if(nargin != 1)
    error("usage: roots(vector)");
  endif

  if(is_matrix(c))
    error("argument must be a vector.");
  endif

  n = length(c);

  if(is_scalar(c) || n == 0)
    r = [];
    return;
  endif

  # Ensure that c is a row vector.
  if(rows(c) > 1)
    c = reshape(c,1,n);
  endif

  # We could replace this with a call to compan, but it's faster to
  # just reproduce the code here.
  A = diag(ones(n-2,1),-1);
  A(1,:) = -c(2:n)/c(1);

  r = eig(A);
 
  # Sort roots in order by decreasing magnitude.
  [mr i] = sort(abs(r));
  r = r(i(length(i):-1:1));

endfunction
