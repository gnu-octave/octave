classdef class_bug66930A

  properties
    bar = {}
  endproperties

  methods
    function out = class_bug66930A (x, varargin)
      out.bar = {x};
    endfunction

    function out = foofcn (x, varargin)
      out = x;
    endfunction

    function out = foofcn (x, varargin)
      out = x;
    endfunction

    function out = foofcn (x, varargin)
      out = x;
    endfunction

  endmethods

endclassdef
