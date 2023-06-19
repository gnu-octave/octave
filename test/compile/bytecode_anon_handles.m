function bytecode_anon_handles ()
  h1 = @(x) __printf_assert__ ("%d ", x);
  h1 (1);
  h1 (2);
  h11 = h1;
  h11 (12);

  a = 3;
  h2 = @() __printf_assert__ ("%d ", a);
  h2 ();

  h3 = @(a,b,c) a + b + c;
  __printf_assert__ ("%d ", h3 (1, 2, 1));

  h4 = @() {1,2,3}{:};
  [a b c] = h4();
  __printf_assert__ ("%d %d %d ", a, b, c);
  [a b] = h4();
  __printf_assert__ ("%d %d ", a, b);
  
  h5 = @(x) @(y) __printf_assert__ ("%d %d ", x, y);
  h5(11)(12)

  % max not in parent scope
  h6 = @(x, y) max (x, y);
  __printf_assert__ ("%d ", h6 (-1, 1));
end