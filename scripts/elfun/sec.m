function w = sec (z)
  
# sec (z): compute the secant for each element of z.

  if (nargin != 1)
    usage ("sec (z)");
  endif
  
  y = 1 ./ cos(z);
  
endfunction