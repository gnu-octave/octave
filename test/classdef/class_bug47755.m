classdef class_bug47755
  properties
    v = 0;
  endproperties
  methods
    function obj = class_bug47755 (x)
      if (nargin > 0)
        obj.v = x;
      endif
    endfunction
    function r = count (obj)
      r = numel (obj);
    endfunction
    function r = add (obj, x)
      r = [numel(obj), x];
    endfunction
    function [p, q] = two (obj)
      p = numel (obj);
      q = 2 * numel (obj);
    endfunction
    function show (obj)
      printf ("%d", numel (obj));
    endfunction
    function r = info (obj)
      r = struct ("x", numel (obj));
    endfunction
    function r = call_hidden (obj)
      r = obj.hidden ();
    endfunction
  endmethods
  methods (Access = private)
    function r = hidden (obj)
      r = numel (obj);
    endfunction
  endmethods
  methods (Static)
    function r = plus100 (x)
      r = x + 100;
    endfunction
  endmethods
endclassdef
