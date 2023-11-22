function bytecode_assign ()

  a = 2;
  __printf_assert__ ("%d ", a);

  a = 3;
  __printf_assert__ ("%d ", a);

  b = a = 1;
  __printf_assert__ ("%d ", a);
  __printf_assert__ ("%d ", b);

  c = [2 2; 3 3];
  d = c (1,2);
  __printf_assert__ ("%d ", c);
  __printf_assert__ ("%d ", d);

  % Compound assignment
  d = 1;
  d += 1;
  __printf_assert__ ("%d ", d);
  d += d * 2;
  __printf_assert__ ("%d ", d);
  d *= 3;
  __printf_assert__ ("%d ", d);
  d /= 9;
  __printf_assert__ ("%f ", d);

  b = [1 2 3 4];
  b += 1;
  __printf_assert__ ("%f ", b);
  __printf_assert__ ("%d ", size(b));
  __printf_assert__ ("%s ", class(b));
  b \= 2;
  b -= 2;
  b *= 2;
  b /= 2;
  b += 2;

  b .\= 2;
  %b .-= 2; % TODO: Removed in interpreter. Remove in VM too.
  b .*= 2;
  b ./= 2;
  %b .+= 2; % TODO: Removed in interpreter. Remove in VM too.
  b .^= 2;

  __printf_assert__ ("%f ", b);
  __printf_assert__ ("%d ", size(b));
  __printf_assert__ ("%s ", class(b));

  b = [1 2; 3 4];
  b ^= 3;

  __printf_assert__ ("%f ", b);
  __printf_assert__ ("%d ", size(b));
  __printf_assert__ ("%s ", class(b));
end
