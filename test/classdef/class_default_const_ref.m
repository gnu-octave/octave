classdef class_default_const_ref
  properties (Constant)
    A = 1;
    B = class_default_const_ref.A * 2;
  endproperties
endclassdef
