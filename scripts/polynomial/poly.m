function y = poly (x)

# If A is a square n-by-n matrix, poly (A) is the row vector of 
# the coefficients of det (z * eye(n) - A), the characteristic
# polynomial of A.
# If x is a vector, poly (x) is a vector of coefficients of the
# polynomial whose roots are the elements of x.

# Written by KH (Kurt.Hornik@neuro.tuwien.ac.at) on Dec 24, 1993 
# Copyright Dept of Probability Theory and Statistics TU Wien

  m = min (size (x));
  n = max (size (x));
  if (m == 0)
    y = 1;
  elseif (m == 1)
    v = x;
  elseif (m == n)
    v = eig (x);
  else
    usage ("poly(x), where x is a vector or a square matrix");
  endif
  
  y = [1, zeros (1, n)];
  for j = 1:n;
    y(2:(j+1)) = y(2:(j+1)) - v(j) .* y(1:j);
  endfor
  
  if (all (all (imag (x) == 0)))
    y = real (y);
  endif
  
endfunction
