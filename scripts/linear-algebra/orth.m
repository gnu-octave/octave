function retval = orth (A, tol)

# usage: orth (A, tol)
#        orth (A)
#
# Returns an orthonormal basis of the range of A.
#
# The dimension of the range space is taken as the number of singular
# values of A greater than tol; the default for tol is
# max (size (A)) * sigma_max (A) * eps, where sigma_max (A) is the
# maximal singular value of A.

# written by KH (Kurt.Hornik@neuro.tuwien.ac.at) on Dec 24, 1993
# copyright Dept of Probability Theory and Statistics TU Wien

  [U, S, V] = svd (A);

  [rows, cols] = size (A);

  s = diag (S);

  if (nargin == 1)
    tol = max (size (A)) * s (1) * eps;
  else if (nargin != 2)
    error ("usage: orth (A [, tol])"); 
  endif

  rank = sum (s > tol);

  if (rank > 0)
    retval = -U (:, 1:rank);
  else
    retval = zeros (rows, 0);
  endif

endfunction
