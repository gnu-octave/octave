classdef class_bug60489
  properties
    a
  endproperties

  methods
    function obj = toto (a)
      obj.a = a
    endfunction

    # double declaration is not flagged as error or warning
    function obj = toto (a)
      obj.a = 50 ;
    endfunction

  endmethods
endclassdef
