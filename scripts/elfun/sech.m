function w = sech (z)
  
# sech (z):  compute the hyperbolic secant for each element of z.
  
  if (nargin != 1)
    usage ("sech (z)");
  endif

  y = 1 ./ cosh(z);
  
endfunction