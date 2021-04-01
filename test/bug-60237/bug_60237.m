function r = bug_60237 ()
  d = 2;
  function c = bm (a)
    c = a + d;
  endfunction;
  r = ancall (@(a) bm (a), 2);
endfunction

function r = ancall (f, a)
  r = f (a) + 1;
endfunction
