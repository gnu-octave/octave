function w = acot (z)

# acot (z):  compute the inverse cotangent for each element of z.

  if (nargin != 1)
    usage ("acot (z)");
  endif

  w = atan (1 ./ z);

endfunction