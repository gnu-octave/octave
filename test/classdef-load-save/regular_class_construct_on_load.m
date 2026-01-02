classdef (ConstructOnLoad) regular_class_construct_on_load < regular_class
  properties (Transient)
    f
  endproperties
  methods
    function obj = regular_class_construct_on_load ()
      obj.a = 1;
      obj.b = 2;
      obj.c = 3;
      obj.d = 4;
      obj.e = 5;
      obj.f = 6;
    endfunction
  endmethods
endclassdef
