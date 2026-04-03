classdef class_bug59775B
  properties
    A
  endproperties
  methods
    function obj = class_bug59775B (a)
      if (nargin > 0)
        if (isscalar (a))
          obj.A = a;
        else
          obj (size (a)) = class_bug59775B ();

          for (kk = 1:numel (a))
            obj(kk).A = a(kk);
          endfor
        endif
      endif
    endfunction
  endmethods
endclassdef
