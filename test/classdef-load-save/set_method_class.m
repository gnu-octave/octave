classdef set_method_class
  ## Not in alphabetical order, so that a test can tell the order of
  ## declaration.
  properties
    c = 0
    b = 0
    a = 0
  endproperties
  properties (Transient)
    trace = ""
  endproperties
  properties (Constant)
    k = 5
  endproperties
  properties (Dependent)
    d
  endproperties
  methods
    ## Each set method records its call in the transient property TRACE.
    function obj = set.a (obj, val)
      obj.trace = [obj.trace, "a"];
      obj.a = val + 100;
    endfunction
    function obj = set.b (obj, val)
      obj.trace = [obj.trace, "b"];
      obj.b = val + 100;
    endfunction
    function obj = set.c (obj, val)
      obj.trace = [obj.trace, "c"];
      obj.c = val + 100;
    endfunction
    function val = get.d (obj)
      val = obj.a;
    endfunction
    function obj = set.d (obj, val)
      obj.trace = [obj.trace, "d"];
    endfunction
  endmethods
endclassdef
