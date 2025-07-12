classdef saveobj_class < regular_class
  methods
    function s = saveobj (obj)
      s.a = obj.a;
      s.b = obj.b;
      s.c = obj.c;
      s.d = obj.d;
      s.e = obj.e;
    endfunction
  endmethods
endclassdef
