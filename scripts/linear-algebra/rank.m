function retval = rank (A, tol)

# usage: rand (a, tol)
#
# Return the rank of the matrix a.  The rank is taken to be the number
# of singular values of a that are greater than tol.
#
# If the second argument is omitted, it is taken to be
#
#   tol =  max (size (a)) * sigma (1) * eps;
#
# where eps is machine precision and sigma is the largest singular
# value of a.

  if (nargin == 1)
    sigma = svd (A);
    tolerance = max (size (A)) * sigma (1) * eps;
  elseif (nargin == 2)
    tolerance = tol;
  else
    error ("usage: rank (A)");
  endif
  retval = sum (sigma > tolerance);

endfunction
