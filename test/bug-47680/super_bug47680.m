classdef super_bug47680
  properties
    tag;
  endproperties
  methods
    function obj = super_bug47680 (x)
      obj.tag = x;
    endfunction
    function r = meth1 (obj, x)
      r = [obj.tag ":super:meth1:" x];
    endfunction
    function r = meth2 (obj, x)
      r = [obj.tag ":super:meth2:" x];
    endfunction
    function r = meth3 (obj, x)
      r = [obj.tag ":super:meth3:" x];
    endfunction
    function r = meth4 (obj, x)
      r = [obj.tag ":super:meth4:" x];
    endfunction
  endmethods
endclassdef
