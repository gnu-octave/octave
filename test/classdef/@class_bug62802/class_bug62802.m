classdef class_bug62802
## Test class with methods in a @folder and in the classdef definition

  methods

    function obj = class_bug62802 ()
    endfunction

    function foo (obj)
    endfunction

    function disp (obj)
      fprintf ("Hello\n");
    endfunction

  endmethods

endclassdef
