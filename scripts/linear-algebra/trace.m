function y = trace (x)

# usage: trace (x)
#
# Returns the trace (the sum of the diagonal elements) of x.

  if (nargin != 1)
    error ("usage: trace (x)");
  endif

  y = sum (diag (x));

endfunction
