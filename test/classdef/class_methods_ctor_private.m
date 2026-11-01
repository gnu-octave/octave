classdef class_methods_ctor_private
  methods (Access = private)
    function obj = class_methods_ctor_private ()
    endfunction
  endmethods
  methods
    function r = foo (this)
      r = 1;
    endfunction
  endmethods
endclassdef
