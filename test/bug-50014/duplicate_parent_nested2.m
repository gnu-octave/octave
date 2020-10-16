## Test that we detect the correct error.
function duplicate_parent_nested2 ()
  function notbug ()
    function bug ()
    endfunction
  endfunction
  function bug ()  # no error here
    function bug ()  # error here (duplicates parent name)
    endfunction
  endfunction
endfunction
