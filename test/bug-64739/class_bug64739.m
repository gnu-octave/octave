classdef class_bug64739
  properties
    a double
    b double = 5;
    c double {mustBePositive} = 1;
    d (1,1) double = 2;
    e uint8 = 3;
    f double = zeros (1, 2);
  endproperties
  properties (Access = private)
    p double = 6;
  endproperties
  properties g double = 4; endproperties
  methods
    function r = after_block (obj)
      bug64739_noop hello
      r = obj.b + obj.p;
    endfunction
  endmethods
endclassdef
