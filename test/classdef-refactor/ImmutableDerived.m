classdef ImmutableDerived < ImmutableValue
  methods
    function obj = ImmutableDerived (rewrite)
      obj@ImmutableValue (21);
      if (nargin > 0 && rewrite)
        obj.value = 0;
      endif
    endfunction
  endmethods
endclassdef
