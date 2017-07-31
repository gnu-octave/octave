classdef class_bug51599 < handle
  properties
    anon_fcn
  endproperties
  methods
    function foo (obj)
      obj.anon_fcn = @(x) my_helper (x + 1);
    endfunction
    function ret = bar (obj, val)
      ret = obj.anon_fcn (val);
    endfunction
  endmethods
endclassdef

function ret = my_helper (val)
  ret = val + 1;
endfunction
