classdef get_set_class
  properties
    a = 1
    b = 2
    c = 3
  endproperties

  properties (Constant)
    d
  endproperties

  methods
    function obj = get_set_class ()
      obj.c = 0;
    endfunction

    function retval = get.a (obj)
      retval = obj.a + 1;
    endfunction

    function obj = set.a (obj, val)
      obj.a = obj.a + val*2;
    endfunction

    # Compute using another get method
    function retval = get.b (obj)
      retval = obj.a + obj.b;
    endfunction

    function retval = get.c (obj)
      retval = obj.c + 5;
    endfunction

    function retval = set.d (obj)
      obj.d = 50;
    endfunction
  endmethods
endclassdef
