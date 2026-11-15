classdef overloaded_subsasgn_class
  properties
    a
  endproperties
  methods
    function obj = overloaded_subsasgn_class (a)
      if (nargin > 0)
        obj.a = a;
      endif
    endfunction
    ## An error for '()', so that a test can tell whether load called this
    ## method to build an object array.
    function obj = subsasgn (obj, s, val)
      if (strcmp (s(1).type, "()"))
        error ("overloaded_subsasgn_class: unexpected '()' assignment");
      endif
      obj = builtin ("subsasgn", obj, s, val);
    endfunction
  endmethods
endclassdef
