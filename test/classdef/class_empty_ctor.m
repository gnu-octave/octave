classdef class_empty_ctor
  properties
    A;
  endproperties
  methods
    function obj = class_empty_ctor (a)
      if (nargin == 0)
        error ("class_empty_ctor: an argument is required");
      endif
      obj.A = a;
    endfunction
  endmethods
endclassdef
