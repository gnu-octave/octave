function retval = is_symmetric (x,tol)

# Usage: is_symmetric (x {,tol})
#
# If x is symmetric, return the dimension of x, otherwise, return 0.
#
# See also: size, rows, columns, length, is_matrix, is_scalar, 
# is_square, is_vector

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if (nargin == 1 || nargin == 2)
    if ((retval = is_square (x)))
      if (nargin == 1)
	tol = eps;
      endif
      if (norm (x - x') / norm(x) > tol)
        retval = 0;
      endif
    endif
  else
    error ("usage: is_symmetric (x {,tol})");
  endif

endfunction
