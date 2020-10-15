## script_nest.m
function x = script_nest
  A (5)
  function A (n)
    if (n <= 0)
      script_nest_script;
    else
      A (n - 1);
    endif
  endfunction
endfunction
