function [X, map] = gray2ind(I,n)

  if(nargin == 1)
    n = 64;
  endif

  map = gray(n);

  X = round(I*(n-1)) + 1;

endfunction
