classdef bug59704_2 < handle

  properties
    data_
  endproperties

  methods

    function obj = bug59704_2 ()
      obj.data_ = 1001:1005;
    endfunction

    function r = subsref (obj, S)
      ## Transform: obj(index) --> obj.data_(index)
      if strcmp (S(1).type, '()')
        r = subsref (obj.data_, S);
      else
        r = builtin('subsref', obj, S);
      endif
    endfunction

    function obj = subsasgn (obj, S, B)
      ## Transform: obj(index) --> obj.data_(index)
      if strcmp (S(1).type, '()')
        obj.data_ = subsasgn (obj.data_, S, B);
      else
        obj = builtin ('subsasgn', obj, S, B);
      endif
    endfunction

  endmethods

endclassdef
