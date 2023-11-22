function bytecode_unary ()
  a = 1;
  a = -a;
  __printf_assert__ ("%d ", a);

  c = +4;
  b = +c;
  __printf_assert__ ("%d ", c);

  a = [1 2; 3 4]';
  __printf_assert__ ("%d ", a);
  a = a';
  __printf_assert__ ("%d ", a);

  b = true;
  b = ~b;
  __printf_assert__ ("%d ", b);

  b = true;
  b = !b;
  __printf_assert__ ("%d ", b);
end
