function retval = reshape (a, m, n)

# usage: reshape (a, m, n)
#
# Form an m x n matrix from the elements of a (taken in Fortran's
# column major ordering).
#
# See also: `:', do_fortran_indexing

  if (nargin != 3)
    error ("usage: reshape (a, m, n)");
  else
    [nr, nc] = size (a);
    if (nr * nc == m * n)
      tmp = do_fortran_indexing;
      do_fortran_indexing = "true";
      retval = zeros (m, n);
      retval (:) = a;
      do_fortran_indexing = tmp;
    else
      error ("reshape: sizes must match");
    endif
  endif

endfunction
