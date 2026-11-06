classdef class_bug47755_base
  methods
    function r = shared (obj)
      r = -numel (obj);
    endfunction
  endmethods
endclassdef
