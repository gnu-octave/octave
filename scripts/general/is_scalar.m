function retval = is_scalar (x)

# usage: is_scalar (x)
#
# Return 1 if the number of rows and columns of x are both equal to 1.
#
# See also: size, rows, columns, length, is_scalar, is_matrix

  if (nargin == 1)
    [nr, nc] = size (x);
    retval = (nr == 1 && nc == 1);
  else
    error ("usage: is_scalar (x)");
  endif

endfunction
