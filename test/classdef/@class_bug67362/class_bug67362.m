classdef class_bug67362
## Test class with property that shares the name with a method in a @folder

  properties
    shared_name = 42;  # property shares name with method in @folder
  endproperties

  methods

    function obj = class_bug67362 ()
    endfunction

  endmethods

endclassdef
