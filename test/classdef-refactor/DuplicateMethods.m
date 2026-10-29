classdef DuplicateMethods
  methods
    function r = query (obj)
      r = 1;
    endfunction
  endmethods
  methods (Static)
    function r = query ()
      r = 2;
    endfunction
  endmethods
endclassdef
