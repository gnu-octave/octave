function w = acoth (z)

# acoth (z):  compute the inverse hyperbolic cotangent for each element of z. 

  if (nargin != 1)
    usage ("acoth (z)");
  endif

  w = atanh (1 ./ z);

endfunction