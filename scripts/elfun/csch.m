function w = csch (z)
  
# csch (z):  compute the hyperbolic cosecant for each element of z.
  
  if (nargin != 1)
    usage ("csch (z)");
  endif

  w = 1 ./ sinh(z);
  
endfunction
