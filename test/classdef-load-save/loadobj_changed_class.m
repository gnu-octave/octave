classdef loadobj_changed_class

    properties
      a
      prop_prior
    endproperties

    methods (Static)
      function obj = loadobj (s)
        obj = loadobj_changed_class ();
        if isstruct (s)
          obj.a = "struct";
        else
          obj.a = "object";
        endif
      endfunction
    endmethods

endclassdef
