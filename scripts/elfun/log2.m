function w = log2 (z)
  
# log2 (z):  compute the logarithm base 2 for each element of z.
  
  if (nargin != 1)
    error ("usage: log2 (z)");
  endif

  w = log(z) / log(2);
  
endfunction