function retval = is_observable (a,c,tol)

# usage: is_observable (a,c{,tol})
#
# returns 1 the pair(a,c) is observable, then return value is the 
# dimension of x.  0therwise, returns a value of 0
#
# See also: size, rows, columns, length, is_matrix, is_scalar, is_vector

  if (nargin == 2)
    retval = is_controllable(a',c');
  elseif (nargin == 3)
    retval = iscontrollable(a',c',tol);
  else
    error ("usage: is_observable (a,c{,tol})");
  endif

endfunction
