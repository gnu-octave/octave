classdef (UnknownClassAttribute) IgnoredAttributes
  properties (UnknownPropertyAttribute)
    value = 42;
  endproperties
  methods (Test)
    function r = query (obj)
      r = obj.value;
    endfunction
  endmethods
endclassdef
