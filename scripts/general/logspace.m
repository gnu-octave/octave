function retval = logspace (x1, x2, n)

# usage: logspace (x1, x2, n)
#
# Return a vector of n logarithmically equally spaced points between
# x1 and x2 inclusive.
#
# If the final argument is omitted, n = 50 is assumed.
#
# All three arguments must be scalars. 
#
# Note that if if x2 is pi, the points are between 10^x1 and pi, NOT
# 10^x1 and 10^pi.
#
# Yes, this is pretty stupid, because you could achieve the same
# result with logspace (x1, log10 (pi)), but Matlab does this, and
# claims that is useful for signal processing applications.
#
# See also: linspace

  if (nargin == 2)
    npoints = 50;
  elseif (nargin == 3)
    if (length (n) == 1)
      npoints = n;
    else
      error ("logspace: arguments must be scalars");
    endif  
  else
    error ("usage: logspace (x1, x2 [, n])");
  endif

  if (npoints < 2)
    error ("logspace: npoints must be greater than 2");
  endif

  if (length (x1) == 1 && length (x2) == 1)
    x2_tmp = x2;
    if (x2 == pi)
      x2_tmp = log10 (pi);
    endif
    retval = linspace (x1, x2_tmp, npoints);
    for i = 1:npoints
      retval(i) = 10 ^ retval(i);
    endfor
  else
    error ("logspace: arguments must be scalars");
  endif

endfunction
