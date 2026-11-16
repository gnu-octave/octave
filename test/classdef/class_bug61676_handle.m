classdef class_bug61676_handle < handle
  properties
    v = 0;
  endproperties
  methods
    function obj = class_bug61676_handle (x)
      if (nargin > 0)
        obj.v = x;
      endif
    endfunction
  endmethods
endclassdef
