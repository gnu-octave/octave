## Test that duplicate names in separate subfunctions are OK.

function retval = duplicate_nested_in_subfunction_ok ()
  retval = sub1 () + sub2 ();
endfunction

function r = sub1 ()
  function notbug ()
    r = 1;
  endfunction
  notbug ();
endfunction

function r = sub2 ()
  function notbug ()
    r = 2;
  endfunction
  notbug ();
endfunction
