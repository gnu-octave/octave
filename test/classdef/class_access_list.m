classdef class_access_list
  properties (SetAccess = {?class_access_list_friend})
    value = 1;
  endproperties
  methods (Access = {?class_access_list_friend})
    function r = secret (obj)
      r = 42;
    endfunction
  endmethods
  methods (Access = ?class_access_list_friend)
    function r = secret_single (obj)
      r = 43;
    endfunction
  endmethods
endclassdef
