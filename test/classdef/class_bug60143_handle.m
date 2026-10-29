classdef class_bug60143_handle < handle

  properties
    t
    u
  endproperties

  methods

    function a = set.t (this, val)
      disp ('method set.t');

      this.t = val;
      a = 1;
    endfunction

    function b = set.u (this, val)
      disp ('method set.u');

      b = class_bug60143_handle;
      b.u = val;
    endfunction

  endmethods

endclassdef
