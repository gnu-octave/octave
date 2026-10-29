classdef PlainPrivate
  properties (Access = private)
    value = 42;
  endproperties
  methods
    function r = call_private (obj)
      r = helper (obj);
    endfunction
    function r = call_nested (obj)
      r = nested_first (obj);
    endfunction
    function f = anonymous_private (obj)
      f = @() helper (obj);
    endfunction
    function f = nested_anonymous_private (obj)
      f = @() @() nested_first (obj);
    endfunction
    function f = private_handle (obj)
      f = @helper;
    endfunction
  endmethods
endclassdef
