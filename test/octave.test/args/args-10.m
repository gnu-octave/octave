1;
function [varargout] = f (varargin)
  printf ("nargin: %d, nargout: %d\n", nargin, nargout);
endfunction
[s, t, u, v] = f (1, 2, 3);
