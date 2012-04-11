function x = varg_nest (varargin)
  x = abs (f (-5)) + g;

  function x = f (varargin)
    x = abs (varargin{1});
  endfunction

  function x = g
    x = abs (varargin{1});
  endfunction
endfunction
