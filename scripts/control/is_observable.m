function retval = is_observable (a,c,tol)

# usage: is_observable (a, c {,tol})
#
# Returns 1 if the pair (a, c) is observable, or 0 if not.
#
# See also: size, rows, columns, length, is_matrix, is_scalar, is_vector.

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if (nargin == 2)
    retval = is_controllable (a', c');
  elseif (nargin == 3)
    retval = is_controllable (a', c', tol);
  else
    error ("usage: is_observable (a, c {,tol})");
  endif

endfunction
