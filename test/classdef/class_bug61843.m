classdef class_bug61843
  properties
    a;
  endproperties
  methods
    function obj = class_bug61843 ()
      obj.a = struct ('b', 1);
    endfunction
  endmethods
endclassdef
