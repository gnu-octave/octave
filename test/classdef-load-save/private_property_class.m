classdef private_property_class
  properties
    a
  endproperties
  properties (Access = protected)
    b
  endproperties
  properties (Access = private)
    c
  endproperties
  methods
    function obj = set_values (obj, a, b, c)
      obj.a = a;
      obj.b = b;
      obj.c = c;
    endfunction
    function r = get_values (obj)
      r = {obj.a, obj.b, obj.c};
    endfunction
  endmethods
endclassdef
