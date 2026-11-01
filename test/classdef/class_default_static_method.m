classdef class_default_static_method
  properties
    B = class_default_static_method.two ();
  endproperties
  properties (Constant)
    C = class_default_static_method.two () + 1;
  endproperties
  methods (Static)
    function r = two ()
      r = 2;
    endfunction
  endmethods
endclassdef
