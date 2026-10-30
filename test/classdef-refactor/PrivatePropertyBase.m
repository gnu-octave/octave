classdef PrivatePropertyBase
  properties (Access = private)
    scalar = 42;
  endproperties
  methods
    function r = base_scalar (obj)
      r = obj.scalar;
    endfunction
  endmethods
endclassdef
