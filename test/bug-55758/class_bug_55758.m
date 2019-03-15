classdef class_bug_55758 < handle
  properties
    value
  endproperties

  methods
    function self = class_bug_55758 (val)
      if (nargin < 1)
        val = 0;
      endif
      self.value = val;
    endfunction

    function delete (self)
      global class_bug_55758_dtor_called;
      class_bug_55758_dtor_called = true;
    endfunction

    function ret = subsref (self, idx)
      ret = self.value;
    endfunction
  endmethods
endclassdef
