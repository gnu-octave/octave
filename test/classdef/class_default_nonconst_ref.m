classdef class_default_nonconst_ref
  properties
    B = class_default_nonconst_ref.A * 2;
  endproperties
  properties (Constant)
    A = 1;
  endproperties
endclassdef
