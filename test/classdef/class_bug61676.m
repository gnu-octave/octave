classdef class_bug61676
  properties
    v = 0;
  endproperties
  methods
    function obj = class_bug61676 (x)
      if (nargin > 0)
        obj.v = x;
      endif
    endfunction
    function r = double (obj)
      r = [obj.v] + 0.5;
    endfunction
    function r = int8 (obj)
      r = int8 (-7);
    endfunction
    function r = int16 (obj)
      error ("class_bug61676: no int16 value");
    endfunction
    function r = uint8 (obj)
      r = obj;
    endfunction
    function r = char (obj)
      r = "#";
    endfunction
    function r = logical (obj)
      r = true;
    endfunction
  endmethods
endclassdef
