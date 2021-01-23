classdef bug59704_1 < handle

  properties
    p
  endproperties

  methods

    function n = numel (this, varargin)
      n = 1;
    endfunction

    function test (this)
      [~, this.p] = bug59704_1_test (this.p);
    endfunction

  endmethods

endclassdef
