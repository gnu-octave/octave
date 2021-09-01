function r = nested_test_1 (val)
  a = 3;
  b = 5;
  c = 7;
  function r = f1 (f, x)
    r = f(x) + a;
  endfunction
  function r = f2 (y)
    function r2 = f3 (z)
      r2 = z^2 + b*y;
    endfunction
    r = f1 (@f3, y) + c;
  endfunction
  r = f2 (val);
endfunction
