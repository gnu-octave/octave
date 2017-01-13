## Test nested function that duplicates nested function name
function duplicate_parent_nested_function ()
  function bug ()
    function bug ()
    endfunction
  endfunction
endfunction
