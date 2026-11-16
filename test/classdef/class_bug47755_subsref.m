classdef class_bug47755_subsref
  methods
    function varargout = subsref (obj, s)
      printf ("%d%s", numel (obj), s(1).type);
      [varargout{1:nargout}] = builtin ("subsref", obj, s);
    endfunction
    function r = count (obj)
      r = numel (obj);
    endfunction
  endmethods
endclassdef
