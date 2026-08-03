classdef class_bug51659B
  properties
    stub_property
  endproperties

  methods
    function [obj, all_methods] = class_bug51659B (self)
      all_methods = properties (obj);
    endfunction
  endmethods
endclassdef
