function c = poly(r)
#Find the coefficients of a polynomial from its roots.
#Find the characteristic polynomial of a matrix.
#
#Given a vector r of length n containing the n roots of polynomial p(x)
#
#  p(x) = (x - r(1)) * (x - r(2)) * ... * (x - r(n))
#
#poly(r) will return a coefficient vector c of length n+1 such that
#
#  p(x) = c(1) x^n + ... + c(n) x + c(n+1).
#
#and c(1) will always be equal to one.
#
#Given a matrix A, poly(A) will return a vector containing the coefficients
#of the characteristic polynomial of A, det(xI - A).
#
#poly and roots are inverse functions to within a scaling factor.
#
#SEE ALSO: roots, conv, deconv, residue, filter, polyval, polyderiv, polyinteg

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  June 1994

  if(nargin != 1)
    error("usage: roots(argument)");
  endif

  l = length(r) + 1;

  if (is_scalar(r))
    c = [1 -r];
    return;
  elseif(l == 1)
    # r is an empty matrix.
    # Matlab compatibility
    c = 1;
    return;
  elseif (is_square(r))
    r = eig(r);
  elseif (is_matrix(r))
    error("poly: matrix argument must be square.");
  endif

  c = ones(1,l);
  c(l) = -r(1);
  for index = 2:(l-1)
    m = l + 2 - index;
    c((m-1):l) = [c(m:l) 0] - r(index)*[1 c(m:l)];
  endfor

endfunction
