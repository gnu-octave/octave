function w = acsch (z)
  
# acsch (z):  compute the inverse hyperbolic cosecant for each element of z.
  
  if (nargin != 1)
    usage ("acsch (z)");
  endif

  w = asinh (1 ./ z);
  
endfunction