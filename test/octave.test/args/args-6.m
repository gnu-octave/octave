1;
function [varargout] = f (x, varargin)
  printf ("nargin: %d, nargout: %d\n", nargin, nargout);
endfunction
f (1)
