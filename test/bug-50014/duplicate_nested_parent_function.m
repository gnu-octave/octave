## Test nested function that duplicates nested function name
function duplicate_nested_parent_function ()
  function bug ()
    function bug ()
    endfunction
  endfunction
endfunction
