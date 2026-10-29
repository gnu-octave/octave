classdef ImmutableValue
  properties (SetAccess = immutable)
    value = [7, 8];
    missing;
  endproperties
  methods
    function obj = ImmutableValue (value, previous)
      if (nargin > 0)
        obj.value = value;
        obj.value(2) = value + 1;
      endif
      if (nargin > 1)
        previous.value = 0;
      endif
    endfunction
    function obj = change (obj)
      obj.value = 0;
    endfunction
  endmethods
endclassdef
