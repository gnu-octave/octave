classdef ImmutableSetter
  properties (SetAccess = immutable)
    value;
  endproperties
  methods
    function obj = set.value (obj, val)
      obj.value = val;
    endfunction
  endmethods
endclassdef
