1;
function [varargout] = f (varargin)
  printf ("nargin: %d, nargout: %d\n", nargin, nargout);
  varargout{1} = (varargin{1});
endfunction
z = f (1);
