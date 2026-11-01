classdef class_negated_static
  methods (~Static)
    function retval = f (obj)
      retval = 1;
    endfunction
  endmethods
  methods (!Static)
    function retval = g (obj)
      retval = 2;
    endfunction
  endmethods
endclassdef
