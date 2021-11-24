## recursive_nest.m
function x = recursive_nest ()
  global recursive_nest_inc = 1
  x = 5;
  f (20);

  function f (n)
    if (n > 0)
      x = x + recursive_nest_inc;
      f (n - 1);
    end
  endfunction
endfunction
