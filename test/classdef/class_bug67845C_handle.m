classdef class_bug67845C_handle < handle
  methods
    function [obj, val1, val2, val3, val4] ...
        = class_bug67845C_handle (val1_in, val2_in, val3_in, val4_in)

      val1 = 1;
      val2 = 2;
      val3 = 3;
      val4 = 4;

      if (nargin >= 1)
        val1 = val1_in;
      endif

      if (nargin >= 2)
        val2 = val2_in;
      endif

      if (nargin >= 3)
        val3 = val3_in;
      endif

      if (nargin >= 4)
        val4 = val4_in;
      endif
    endfunction
  endmethods
endclassdef
