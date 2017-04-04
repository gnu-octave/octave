classdef class_bug41723 < handle
  properties
    value
  endproperties
  methods
    function obj = class_bug41723 ()
      obj.value = helper_function ();
    endfunction
    function val = getval (obj)
      val = helper_function_2 ();
    endfunction
  endmethods
endclassdef

function y = helper_function ()
  y = 42;
endfunction

function y = helper_function_2 (in)
  if (! nargin)
    y = helper_function_2 (true);
  else
    y = 2 * helper_function ();
  endif
endfunction
