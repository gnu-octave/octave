function len = length (x)

# usage: length (x)
#
# Return the number of rows or columns, whichever is greater.
#
# See also: size, rows, columns, is_scalar, is_vector, is_matrix

  if (nargin != 1)
    error ("usage: length (x)");
  endif

  len = max (size (x));

endfunction
