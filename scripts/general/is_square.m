function retval = is_square (x)

# usage: is_square (x)
#
# If x is square, then return value is the dimension of x.
# otherwise, returns a value of 0
#
# See also: size, rows, columns, length, is_matrix, is_scalar, is_vector

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if (nargin == 1)
    [nr, nc] = size (x);
    if (nr == nc) 
      retval = nr;
    else
      retval = 0;
    endif
  else
    error ("usage: is_square (x)");
  endif

endfunction
