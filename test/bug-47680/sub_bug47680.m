classdef sub_bug47680 < super_bug47680
  methods
    function obj = sub_bug47680 (x)
      obj = obj@super_bug47680 (x);
    endfunction
    function r = meth1 (obj)
      r = meth1@super_bug47680 (obj, "sub:meth1");
    endfunction
    function r = meth2 (obj)
      r = obj.meth2@super_bug47680 ("sub:meth2");
    endfunction
    function r = meth3 (obj)
      r = meth3@super_bug47680 (obj, "sub:meth3");
    endfunction
    function r = meth4 (obj)
      r = obj.meth4@super_bug47680 ("sub:meth4");
    endfunction
  endmethods
endclassdef
