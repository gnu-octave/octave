classdef class_bug46497 < handle
  properties
    myprop;
  endproperties
  methods
    function obj = class_bug46497 (x)
      obj.myprop = x;
    endfunction
    function delete (self)
      global __bug46497_global__
      __bug46497_global__ = struct ("myprop", self.myprop, "status", "deleted");
    endfunction
  endmethods
endclassdef
