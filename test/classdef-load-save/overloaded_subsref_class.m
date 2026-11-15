classdef overloaded_subsref_class
  properties
    a
  endproperties
  methods
    function obj = overloaded_subsref_class (a)
      if (nargin > 0)
        obj.a = a;
      endif
    endfunction
    ## Wrong values for '.' and '()', an error for '{}', so that a test
    ## can tell whether save or load called this method.
    function r = subsref (obj, s)
      switch (s(1).type)
        case "."
          r = -999;
        case "()"
          r = overloaded_subsref_class (-1);
        otherwise
          error ("overloaded_subsref_class: unexpected '%s' index",
                 s(1).type);
      endswitch
    endfunction
    ## Read the stored values without calling subsref.
    function r = raw_a (obj)
      r = [obj.a];
    endfunction
  endmethods
endclassdef
