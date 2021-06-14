classdef bug_60763
  properties
    some_property;
  endproperties
  methods
    function p = foobar (instance)
      p = properties (instance);
    endfunction
    function m = methods (instance)
      m = 42;
    endfunction
  endmethods
endclassdef
