classdef class_bug67845A_handle < handle
  properties
    prop
  endproperties

  methods
    function [obj, val_out] = class_bug67845A_handle (val_in)
      if (nargin < 1)
        val_out = 0;
        obj.prop = 0;
      else
        val_out = val_in;
        obj.prop = val_in;
      endif
    endfunction
  endmethods
endclassdef
