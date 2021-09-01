function r = nested_test_2 ()
  a = 3;
  b = 5;
  c = 7;
  f1 = @(f, x) f(x) + a;
  f2 = @(y) f1 (@(z) z^2 + b*y, y) + c;
  r = f2 (2);
endfunction
