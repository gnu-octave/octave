function y = poly (x)
#
# If A is a square matrix, poly (A) is the row vector of coefficients of
# the characteristic polynomial det (z * eye(A) - A).
# If x is a vector, poly (x) is a vector of coefficients of the polynomial
# whose roots are the elements of x.

# written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Dec 24, 1993
# copyright Dept of Probability Theory and Statistics TU Wien

  m = min(size(x));
  n = max(size(x));
  if (m == 0)
    y = 1;
  elseif (m == 1)
    v = x;
  elseif (m == n)
    v = eig(x);
  else
    error("usage:  poly(x), where x is a vector or a square matrix");
  endif
  
  y = [ 1 zeros(1,n) ];
  for j = 1:n;
    y(2:(j+1)) = y(2:(j+1)) - v(j) .* y(1:j);
  endfor
  
  if all(imag(x) == 0)
    y = real(y);
  endif
  
endfunction
