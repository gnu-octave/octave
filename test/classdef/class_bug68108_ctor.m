classdef class_bug68108_ctor
  properties
    A = 0;
  endproperties
  methods
    function obj = class_bug68108_ctor (n)
      if (nargin > 0)
        obj(n) = class_bug68108_ctor ();
        val = num2cell (1:n);
        [obj.A] = val{:};
      endif
    endfunction
  endmethods
endclassdef
