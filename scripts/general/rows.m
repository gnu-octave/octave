function nr = rows (x)

# usage: rows (x)
#
# Return the the number of rows in x.
#
# See also: size, columns, length, is_scalar, is_vector, is_matrix

  if (nargin != 1)
    error ("usage: rows (x)");
  endif

  [nr, nc] = size (x);

endfunction
