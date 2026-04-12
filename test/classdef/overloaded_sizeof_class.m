classdef overloaded_sizeof_class
  properties
    a
  endproperties

  methods
    function retval = sizeof (obj)
      retval = -5;  # Impossible for sizeof to return a negative normally
    endfunction
  endmethods
endclassdef
