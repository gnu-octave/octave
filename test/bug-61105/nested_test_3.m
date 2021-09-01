function fh = nested_test_3 ()
  a = 3;
  b = 5;
  c = 7;
  f1 = @(f, x) f(x) + a;
  fh = @(y) f1 (@(z) z^2 + b*y, y) + c;
endfunction
