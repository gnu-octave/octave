function w = asech (z)
  
# asech (z):  compute the inverse hyperbolic secant for each element of z.
  
  if (nargin != 1)
    error ("usage: acosh (z)");
  endif

  w = acosh (1 ./ z);
  
endfunction
