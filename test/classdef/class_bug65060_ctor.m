classdef class_bug65060_ctor
  properties
    p = zeros (1, 3);
  endproperties
  properties (SetAccess = immutable)
    q = zeros (1, 3);
  endproperties
  methods
    function obj = class_bug65060_ctor ()
      for k = 1:3
        obj.p(k) = k;
        obj.q(k) = 2 * k;
      endfor
    endfunction
  endmethods
endclassdef
