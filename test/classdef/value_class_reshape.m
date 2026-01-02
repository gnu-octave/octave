classdef value_class_reshape
  properties
    a
    b
    c
    d
    e
  endproperties

  methods
    function this = reshape(this, new_size)
      this(end,end).a = new_size;
    endfunction
  endmethods
endclassdef
