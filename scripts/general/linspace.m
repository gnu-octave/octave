function retval = linspace (x1, x2, n)

# usage: linspace (x1, x2, n)
#
# Return a vector of n equally spaced points between x1 and x2
# inclusive. 
#
# If the final argument is omitted, n = 100 is assumed.
#
# All three arguments must be scalars.
#
# See also: logspace

  if (nargin == 2)
    npoints = 100;
  elseif (nargin == 3)
    if (length (n) == 1)
      npoints = n;
    else
      error ("linspace: arguments must be scalars");
    endif
  else
    error ("usage: linspace (x1, x2 [, n])");
  endif

  if (npoints < 2)
    error ("linspace: npoints must be greater than 2");
  endif

  if (length (x1) == 1 && length (x2) == 1)
    delta = (x2 - x1) / (npoints - 1);
    retval = zeros (1, npoints);
    for i = 0:npoints-1
      retval (i+1) = x1 + i * delta;
    endfor
  else
    error ("linspace: arguments must be scalars");
  endif

endfunction
