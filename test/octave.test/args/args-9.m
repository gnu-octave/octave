1;
function [x, y, z] = f (a, b, c, d, e)
  printf ("nargin: %d, nargout: %d\n", nargin, nargout);
endfunction
[s, t] = f (1, 2, 3, 4);
