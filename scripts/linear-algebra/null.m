function retval = null (A, tol)

# usage: null (A, tol)
#        null (A)
#
# Returns an orthonormal basis of the null space of A.
#
# The dimension of the null space is taken as the number of singular
# values of A not greater than tol;  the default for tol is
# max (size (A)) * sigma_max (A) * eps, where sigma_max (A) is the
# maximal singular value of A. 

# written by KH (Kurt.Hornik@neuro.tuwien.ac.at) on Dec 24, 1993
# copyright Dept of Probability Theory and Statistics TU Wien

  [U, S, V] = svd (A);

  [rows, cols] = size (A);

  s = diag (S);

  if (nargin == 1)
    tol = max (size (A)) * s (1) * eps;
  else (nargin != 2)
    error("usage: null(A [, tol])"); 
  endif

  rank = sum (s > tol);

  if (rank < cols)
    retval = V (:, rank+1:cols);
  else
    retval = zeros (cols, 0);
  endif

endfunction
