classdef class_default_private
  properties
    v
  endproperties
  properties (Constant, Access = private)
    K = class_default_private.seven ();
    L = class_default_private.K;
  endproperties
  methods
    function obj = class_default_private ()
      obj.v = class_default_private.L;
    endfunction
  endmethods
  methods (Static, Hidden)
    function r = seven ()
      r = 7;
    endfunction
  endmethods
endclassdef
