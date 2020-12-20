classdef bug59661 < handle

  properties
    p = [];
  endproperties

  methods

    function o = bug59661 (varargin)
      o.p = [varargin{:}];
    endfunction

    function res = eq (a, b)

      if (numel (a.p) > 1 && numel (b.p) > 1 && ! isequal (size (a.p), size (b.p)))
        res = false;
        return;
      endif

      if (isequal (size (a.p), size (b.p)))
        res = eq (a.p, b.p);
      else
        res = bsxfun (@eq, a.p, b.p);
      endif

    endfunction

  endmethods
endclassdef
