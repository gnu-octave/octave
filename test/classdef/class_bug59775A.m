classdef class_bug59775A
  properties
    A = 1
  endproperties
  methods
    function obj = class_bug59775A (a)
      if (nargin > 0)
        if isscalar (a)
          obj.A = a;
        else
          obj (size (a)) = class_bug59775A ();
        endif
      endif
    endfunction
  endmethods
endclassdef
