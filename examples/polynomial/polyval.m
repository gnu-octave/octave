function [y, dy] = polyval (p, varargin)
  if (nargout == 2)
    [y, dy] = polyval (p.poly, varargin{:});
  else
    y = polyval (p.poly, varargin{:});
  endif
endfunction
