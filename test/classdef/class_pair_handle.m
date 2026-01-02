classdef class_pair_handle < handle
  properties
    first
    second
  endproperties
  methods
    function this = class_pair_handle (first, varargin)
      if (isa (first, 'class_pair_elem_handle'))
        this.first = first.value;
        this.second = 0;
      else
        this.first = first;
        this.second = varargin{1};
      endif
    endfunction
    function pe = class_pair_elem_handle (this)
      pe = class_pair_elem_handle (this.first);
    endfunction
  endmethods
endclassdef
