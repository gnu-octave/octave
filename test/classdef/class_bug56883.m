classdef class_bug56883
  methods
    function names = method_names (obj, x, z)
      names = cell (1, nargin);
      for k = 1:nargin
        names{k} = inputname (k);
      endfor
    endfunction
  endmethods
  methods (Static)
    function names = static_names (x, z)
      names = cell (1, nargin);
      for k = 1:nargin
        names{k} = inputname (k);
      endfor
    endfunction
  endmethods
endclassdef
