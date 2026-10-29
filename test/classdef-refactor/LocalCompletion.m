classdef LocalCompletion
  methods (Static)
    function matches = complete ()
      matches = complete_local ();
    endfunction
    function f = local_handle ()
      f = @complete_local;
    endfunction
    function matches = nested ()
      matches = complete_nested ();
    endfunction
  endmethods
endclassdef

function matches = complete_local ()
  matches = completion_matches ('hel');
endfunction

function matches = complete_nested ()
  matches = inner ();
  function r = inner ()
    r = completion_matches ('hel');
  endfunction
endfunction
