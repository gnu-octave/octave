classdef class_bug61676_src
  properties
    v = 99;
  endproperties
  methods
    function obj = class_bug61676_src (x)
      if (nargin > 0)
        obj.v = x;
      endif
    endfunction
    function r = class_bug61676 (obj)
      r = class_bug61676 ([obj.v]);
    endfunction
    function r = class_bug61676_handle (obj)
      r = 7;
    endfunction
  endmethods
endclassdef
