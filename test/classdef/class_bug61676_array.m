classdef class_bug61676_array
  properties
    v = 0;
  endproperties
  methods
    function obj = class_bug61676_array (x)
      if (nargin > 0)
        for k = numel (x):-1:1
          obj(k).v = x(k);
        endfor
      endif
    endfunction
  endmethods
endclassdef
