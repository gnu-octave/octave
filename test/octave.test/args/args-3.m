1;
function [x, y] = f ()
  printf ("nargin: %d, nargout: %d\n", nargin, nargout);
  x = 2;
endfunction
x = f ();
