function retval = isempty (var)

# usage: isempty (x)
#
# Return 1 if the argument is an empty matrix.  Otherwise, return 0.

  if (nargin != 1)
    error ("usage: isempty (var)");
  endif

  retval = (rows (var) == 0 || columns (var) == 0);

endfunction
