function x = recursive_nest3 ()
  y = 5;
  f (y);
  x = y;
  g (x);
  function f (y)
    y = 10;
  endfunction

  function g (x)
    x = 10;
  endfunction
endfunction
