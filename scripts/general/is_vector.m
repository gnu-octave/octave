function retval = is_vector (x)

# usage: is_vector (x)
#
# Return 1 if the either the number of rows (columns) of x is 1 and
# the number of columns (rows) is greater than one.  Otherwise, return 0. 
#
# See also: size, rows, columns, length, is_scalar, is_matrix

  if (nargin == 1)
    [nr, nc] = size (x);
    retval = ((nr == 1 && nc > 1) || (nc == 1 && nr > 1));
  else
    error ("usage: is_vector (x)");
  endif

endfunction
