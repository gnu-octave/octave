function [y, dy] = polyval (p, varargin)

  if (nargout > 1)
    [y, dy] = polyval (fliplr (p.poly), varargin{:});
  else
    y = polyval (fliplr (p.poly), varargin{:});
  endif

endfunction
