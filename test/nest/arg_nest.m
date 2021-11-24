## arg_nest.m
function x = arg_nest
  x = 1;
  A (x);
  function A (x)
    x = 2;
  endfunction
endfunction
