function w = cot (z)
  
# cot (z):  compute the cotangent for each element of z.
  
  if (nargin != 1)
    error ("usage: cot (z)");
  endif

  w = 1 ./ tan(z);
  
endfunction