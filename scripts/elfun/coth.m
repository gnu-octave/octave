function w = coth (z)
  
# coth (z):  compute the hyperbolic cotangent for each element of z.
  
  if (nargin != 1)
    error ("usage: coth (z)");
  endif

  w = 1 ./ tanh(z);
  
endfunction