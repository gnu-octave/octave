function p = polyfit (x, y, n)
  
# usage:  polyfit (x, y, n)
#
# Returns the coefficients of a polynomial p(x) of degree n that
# minimizes sumsq (p(x(i)) - y(i)), i.e., that best fits the data 
# in the least squares sense.
  
# Written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Dec 13, 1994
# Copyright Dept of Statistics and Probability Theory TU Wien
  
  if (nargin != 3)
    usage ("polyfit (x, y, n)");
  endif
  
  if (! (is_vector (x) && is_vector (y) && size (x) == size (y)))
    error ("polyfit: x and y must be vectors of the same size");
  endif
  
  if (! (is_scalar (n) && n >= 0 && ! isinf (n) && n == round (n)))
    error ("polyfit: n must be a nonnegative integer");
  endif
  
  l = length (x);
  x = reshape (x, l, 1);
  y = reshape (y, l, 1);
  
  X = ones (l, 1);

  if (n > 0)
    tmp = (x * ones (1, n)) .^ (ones (l, 1) * (1 : n));
    X = [X, tmp];
  endif

  # Compute polynomial coeffients, making returned value compatible
  # with Matlab.

  [Q, R] = qr (X, 0);

  p = flipud (R \ (Q' * y));

  if (! prefer_column_vectors)
    p = p';
  endif

endfunction
