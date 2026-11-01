classdef class_default_cycle
  properties (Constant)
    A = class_default_cycle.B;
    B = class_default_cycle.A;
  endproperties
endclassdef
