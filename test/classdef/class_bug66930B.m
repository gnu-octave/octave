classdef class_bug66930B

  properties
    bar = {}
    bar = {}
    bar = {}
    bar = {}
    bar = {}
  endproperties

  methods
    function out =  class_bug66930B (x, varargin)
      out.bar = {x};
    endfunction

    function out = foofcn1 (x, varargin)
      out = x;
    endfunction

    function out = foofcn2 (x, varargin)
      out = x;
    endfunction

    function out = foofcn3 (x, varargin)
      out = x;
    endfunction

  endmethods

endclassdef
