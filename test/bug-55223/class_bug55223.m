classdef class_bug55223<handle
  properties
    x
  endproperties

  methods
    function obj = class_bug55223 ()
      obj.x = eye (4);
    endfunction

    function val = subsref (obj, S)
      if (length (S) == 2 && S(1).type == "{}" && S(2).type == "()")
        val = obj.x(S(1).subs{1}, S(2).subs{1});
      endif
    endfunction

    function obj = subsasgn (obj, S, val)
      if (length (S) == 2 && S(1).type == "{}" && S(2).type == "()")
        obj.x(S(1).subs{1}, S(2).subs{1}) = val;
      endif
    endfunction
  endmethods
endclassdef
