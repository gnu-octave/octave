global g = -1;
function f (x)
  global g;
  save_g = g;
  unwind_protect
    g = 0
    [1,2;x];
    g = 1
  unwind_protect_cleanup
    g = save_g
  end_unwind_protect
endfunction
f (3)
