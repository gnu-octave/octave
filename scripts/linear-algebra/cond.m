function retval = cond (a)

# usage: cond (a)
#
# Return the condition number of a, computed using the singular values
# of a.
#
# See also: norm, svd

  if (nargin == 1)
    [nr, nc] = size (a);
    if (nr == 0 && nc == 0)
      if (strcmp (propagate_empty_matrices, "false"))
        error ("cond: empty matrix is invalid as argument");
      endif
      if (strcmp (propagate_empty_matrices, "warn"))
        printf ("warning: cond: argument is empty matrix\n");
      endif
      retval = 0.0;
    endif
    sigma = svd (a);
    retval = sigma (1) / sigma (length (sigma));
  else
    error ("usage: cond (a)");
  endif

endfunction
