1;
function [x, y] = f (a, b)
  printf ("nargin: %d, nargout: %d\n", nargin, nargout);
  x = a;
endfunction
z = f (1);
