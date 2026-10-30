classdef (Hidden = isunix) AttributeExpression
  properties (Hidden = isunix)
    value = 42;
  endproperties
  methods (Hidden = isunix)
    function r = query (obj)
      r = obj.value;
    endfunction
  endmethods
endclassdef
