function w = csc (z)
  
# csc (z):  compute the cosecant for each element of z.
  
  if (nargin != 1)
    usage ("csc (z)");
  endif

  w = 1 ./ sin(z);
  
endfunction
