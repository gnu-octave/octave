function obj = osc (data)
  if (nargin == 0)
    data = 0;
  endif

  s = struct;
  s.data = data;
  obj = class (s, 'osc');
endfunction
