classdef bug51567 < handle
  properties
    fh1;
    fh2;
  endproperties
  methods
    function obj = bug51567 (self)
      obj.fh1 = str2func ("bar");
      obj.fh2 = @baz;
    endfunction
    function r = bar (obj)
      r = 13;
    endfunction
    function r = baz (obj)
      r = 42;
    endfunction
    function r = doit_1 (obj)
      r = obj.fh1 ();
    endfunction
    function r = doit_2 (obj)
      r = obj.fh2 ();
    endfunction
  endmethods
endclassdef
