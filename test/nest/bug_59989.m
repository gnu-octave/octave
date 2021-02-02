function y = bug_59989 ()
  unpacker = @(x) sum (x);
  function y = nest1 (x, a)
    y = a + unpacker (x);
  endfunction
  function y = nest2 (fh)
    x = [2, 3];
    y = fh (x);
  endfunction
  a = 1;
  y = nest2 (@(x) nest1 (x, a));
endfunction
