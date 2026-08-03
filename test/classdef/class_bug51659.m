classdef class_bug51659 < handle
  properties
    a
  endproperties

  methods
    function obj = class_bug51659 (obj)
      obj.a = methods (obj);
    endfunction

    function retval = test_method (self)
      retval = methods (self);
    endfunction
  endmethods
endclassdef
