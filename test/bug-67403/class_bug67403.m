classdef class_bug67403
  methods
    function [varargout] = subsref (obj, s)
      varargout = cell(1, max (nargout, 1));
      varargout(:) = {nargout};
    endfunction
    function n = numArgumentsFromSubscript(obj, s, indexingContext)
      n = 2;
    endfunction
  endmethods
endclassdef
