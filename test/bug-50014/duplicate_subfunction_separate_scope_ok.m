## Test that nested functions with the same name are OK if they are
## defined inside other nested functions with different names.

function retval = duplicate_subfunction_separate_scope_ok ()
  function r = suba ()
    function sub2 ()
      r = 1;
    endfunction
    sub2 ();
  endfunction
  function subb ()
    function sub2 ()
      y = 2;
    endfunction
    sub2 ();
  endfunction
  subb ();
  retval = y + suba ();
endfunction
