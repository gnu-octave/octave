function retval = is_matrix (x)

# usage: is_matrix (x)
#
# Return 1 if the number of rows and columns of x are both greater
# than 1.
#
# See also: size, rows, columns, length, is_scalar, is_vector

  if (nargin == 1)
    [nr, nc] = size (x);
    retval = (nr > 1 && nc > 1);
  else
    error ("usage: is_matrix (x)");
  endif

endfunction
