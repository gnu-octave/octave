classdef class_bug51659B_handle < handle
  properties
    stub_property
  endproperties

  methods
    function [obj, all_methods] = class_bug51659B_handle (self)
      all_methods = properties (obj);
    endfunction
  endmethods
endclassdef
