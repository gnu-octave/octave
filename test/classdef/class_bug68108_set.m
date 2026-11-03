classdef class_bug68108_set
  properties
    A = 0;
  endproperties
  methods
    function obj = set.A (obj, val)
      obj.A = 10 * val;
    endfunction
  endmethods
endclassdef
