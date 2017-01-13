## Test duplicate nested function names
function duplicate_nested_function ()

  function r = nested_fcn_1 ()
    r = 1;
  endfunction

  function r = nested_fcn_1 ()
    r = 2;
  endfunction

endfunction
