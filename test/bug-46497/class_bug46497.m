classdef class_bug46497 < handle
  methods
    function delete (self)
      global __bug46497_global__
      __bug46497_global__ = 'deleted';
    endfunction
  endmethods
endclassdef
