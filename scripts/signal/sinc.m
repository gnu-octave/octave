function result = sinc (x)

# usage: sinc(x)
#
#        Returns sin(pi*x)/(pi*x).

# We either need to set the do_fortran_indexing variable to "true"
# or use reshape to convert the input matrix to a vector, so that
# we can use find to determine the elements of x that equal zero.
# I prefer reshaping.

  [nr, nc] = size(x);

  nels = nr*nc;

  x = reshape(x,nels,1);

# Set result to all ones initially.
  result = ones(nels,1);

# Find non-zero elements in the input matrix.
  i = find(x);

  if (!isempty(i))
    result(i) = sin(pi*x(i))./(pi*x(i));
  endif

  result = reshape(result,nr,nc);

endfunction
