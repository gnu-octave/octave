classdef private_property_loadobj_class < private_property_class
  methods (Static)
    ## The saved properties match the class, so loadobj receives an object.
    function obj = loadobj (s)
      if (isstruct (s))
        error ("private_property_loadobj_class: loadobj received a struct");
      endif
      obj = s;
    endfunction
  endmethods
endclassdef
