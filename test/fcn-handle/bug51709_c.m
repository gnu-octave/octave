classdef bug51709_c
  methods
    function out = meth (varargin)
      out = {"meth", varargin};
    endfunction
  endmethods
  methods (Static)
    function out = smeth (varargin)
      out = {"smeth", varargin};
    endfunction
  endmethods
endclassdef
