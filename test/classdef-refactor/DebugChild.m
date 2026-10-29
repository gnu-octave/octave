classdef DebugChild < DebugBase
  properties
    value = 2;
  endproperties
  methods
    function r = child_method (obj)
      r = obj.value;
    endfunction
  endmethods
endclassdef
