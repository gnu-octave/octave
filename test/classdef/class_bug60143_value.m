classdef class_bug60143_value

  properties
    t
    u
  endproperties

  methods

    function set.t (this, val)
      this.t = val;
    endfunction

    function [this, a] = set.u (this, val)
      this.u = val;
      disp ('method set.u');
      a = 1;
    endfunction

  endmethods

endclassdef
