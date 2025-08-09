classdef loadobj_class < regular_class
  methods (Static)
    function obj = loadobj (s)
      obj = loadobj_class ();
      obj.a = s.a;
      obj.b = s.b;
      obj.c = s.c;
      obj.d = s.d;
      obj.e = s.e;
    endfunction
  endmethods
endclassdef
