classdef ImmutablePackage
  properties (SetAccess = immutable)
    value;
  endproperties
  methods
    function obj = ImmutablePackage (value)
      obj.value = value;
    endfunction
  endmethods
endclassdef
