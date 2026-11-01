classdef class_default_fail
  properties (Constant)
    A = 1;
    B = class_default_fail.A + class_default_fail_undefined ();
  endproperties
endclassdef
