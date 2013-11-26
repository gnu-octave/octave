function x = nest_eval (a, b)
  eval (a);
  nested ();

  function nested ()
    eval (b);
  endfunction
endfunction
