function w = asec (z)
  
# asec (z):  compute the inverse secant for each element of z.
  
  if (nargin != 1)
    usage ("asec (z)");
  endif

  w = acos (1 ./ z);
  
endfunction