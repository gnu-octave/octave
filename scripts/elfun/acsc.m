function w = acsc (z)
  
# acsc (z):  compute the inverse cosecant for each element of z.
  
  if (nargin != 1)
    error ("usage: acsc (z)");
  endif

  w = asin (1 ./ z);
  
endfunction