1;
function [x, varargout] = f (varargin)
  printf ("nargin: %d, nargout: %d\n", nargin, nargout);
  x = 2;
endfunction
x = f ();
