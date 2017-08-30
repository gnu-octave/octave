classdef class_bug49379 < handle
  properties
    m_ctor_arg_names = {};
    m_meth_arg_names = {};
  endproperties
  methods
    function obj = class_bug49379 (x, y)
      obj.m_ctor_arg_names = {inputname(1), inputname(2)};
    endfunction
    function meth (obj, x, y)
      obj.m_meth_arg_names = {inputname(1), inputname(2), inputname(3)};
    endfunction
    function names = ctor_arg_names (obj)
      names = obj.m_ctor_arg_names;
    endfunction
    function names = meth_arg_names (obj)
      names = obj.m_meth_arg_names;
    endfunction
  endmethods
endclassdef
