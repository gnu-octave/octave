function y = polyval(c,x)

# Evaluate a polynomial.
# 
# In octave, a polynomial is represented by it's coefficients (arranged
# in descending order). For example a vector c of length n+1 corresponds
# to the following nth order polynomial
# 
#   p(x) = c(1) x^n + ... + c(n) x + c(n+1).
# 
# polyval(c,x) will evaluate the polynomial at the specified value of x.
# 
# If x is a vector or matrix, the polynomial is evaluated at each of the
# elements of x.
# 
# SEE ALSO: polyvalm, poly, roots, conv, deconv, residue, filter,
#           polyderiv, polyinteg

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  June 1994

  if(nargin != 2)
    usage ("polyval(c,x)");
  endif

  if(is_matrix(c))
    error("poly: first argument must be a vector.");
  endif

  if(length(c) == 0)
    y = c;
    return;
  endif

  n = length(c);
  y = c(1)*ones(rows(x),columns(x));
  for index = 2:n
    y = c(index) + x .* y;
  endfor
endfunction
