classdef class_methods_ctor_hidden
  methods (Hidden)
    function obj = class_methods_ctor_hidden ()
    endfunction
  endmethods
  methods
    function r = foo (this)
      r = 1;
    endfunction
  endmethods
endclassdef
