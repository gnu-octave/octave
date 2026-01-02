classdef loadobj_changed_class

    properties
      a
      prop_later
    endproperties

    methods (Static)
      function obj = loadobj (s)
        obj = loadobj_changed_class ();
        if isstruct (s)
          obj.a = "changed-struct";
        else
          obj.a = "changed-object";
        endif
      endfunction
    endmethods

endclassdef
