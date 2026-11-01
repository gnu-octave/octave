classdef class_bug60791
  properties
    p
  endproperties
  methods
    function obj = class_bug60791 (val)
      obj.p = val;
    endfunction
    function obj = subsasgn (obj, s, val)
    endfunction
  endmethods
endclassdef
