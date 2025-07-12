classdef loadobj_saveobj_class < regular_class
  methods (Static)
    function obj = loadobj (s)
      obj = loadobj_class ();
      if (isstruct (s))
        obj.a = s.a;
        obj.b = s.b;
        obj.c = s.c;
        obj.d = s.d;
        obj.e = s.e;
      endif
    endfunction
  endmethods

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
