function nc = columns (x)

# usage: columns (x)
#
# Return the the number of columns in x.
#
# See also: size, rows, length, is_scalar, is_vector, is_matrix

  if (nargin != 1)
    error ("usage: columns (x)");
  endif

  [nr, nc] = size (x);

endfunction
