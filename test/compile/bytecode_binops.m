function bytecode_binops()
  % Root level binary expressions8
  % should not mess up the operand stack
  2 * 3 + 1;
  max (3, 2) - min (3, 2);
  sin (3) * sin (2);

  % General
  a = 1 + 2 + 3 + 4;
  __printf_assert__ ("%.17g ", a);
  a = -1 - 2 - 3 - 4;
  __printf_assert__ ("%.17g ", a);
  a = 1 * 2 * 3 * 4;
  __printf_assert__ ("%.17g ", a);
  a = 1 / 2 / 3 / 4;
  __printf_assert__ ("%.17g ", a);
  a = 1^2^3^4;
  __printf_assert__ ("%.17g ", a);

  % Order
  a = 1 + 2 - 3 * 4 / 5 ^ 6 * 7 / 8 - 9 + 10 / 11;
  __printf_assert__ ("%.17g ", a);

  % Function calls
  a = max (3, 2) * min (2, 1) + max (10, 9);
  __printf_assert__ ("%.17g ", a);

  % Logical
  a = 1 && 2;
  __printf_assert__ ("%d ", a);
  a = 1 && 0;
  __printf_assert__ ("%d ", a);
  a = 1 || 2;
  __printf_assert__ ("%d ", a);
  a = 0 || 0;
  __printf_assert__ ("%d ", a);

  % Need to not linger on stack
  1 && 1;
  1 && 0;
  0 && 1;
  0 || 0;
  0 || 1;
  1 || 0;

  % We wanna make sure there actually is a short circuit
  % and that the operands are only evaluated once
  a = truthy (1) || falsy(2);
  __printf_assert__ ("%d ", a);

  a = falsy (3) || falsy (4) || truthy (5) || falsy (6);
  __printf_assert__ ("%d ", a);

  a = truthy (7) && truthy (8) || falsy (12);
  __printf_assert__ ("%d ", a);

  a = falsy (9) && truthy (10) || falsy (11);
  __printf_assert__ ("%d ", a);

  % Compares

  a = 1 == 1;
  __printf_assert__ ("%d ", a);
  a = 1 == 2;
  __printf_assert__ ("%d ", a);
  a = 1 < 2;
  __printf_assert__ ("%d ", a);
  a = 2 < 1;
  __printf_assert__ ("%d ", a);
  a = 1 > 2;
  __printf_assert__ ("%d ", a);
  a = 2 > 1;
  __printf_assert__ ("%d ", a);
  a = 1 <= 2;
  __printf_assert__ ("%d ", a);
  a = 2 <= 1;
  __printf_assert__ ("%d ", a);
  a = 1 <= 1;
  __printf_assert__ ("%d ", a);
  a = 1 >= 2;
  __printf_assert__ ("%d ", a);
  a = 2 >= 1;
  __printf_assert__ ("%d ", a);
  a = 1 >= 1;
  __printf_assert__ ("%d ", a);
  a = 2 ~= 1;
  __printf_assert__ ("%d ", a);
  a = 1 ~= 1;
  __printf_assert__ ("%d ", a);
  a = 2 != 1;
  __printf_assert__ ("%d ", a);
  a = 1 != 1;
  __printf_assert__ ("%d ", a);

  a = 1 == 1 && 2 > 1 && 3 > -3 || 1 < 10;
  __printf_assert__ ("%d ", a);

  % Elementwise logical

  a = ones (2,2) & zeros (2,2);
  __printf_assert__ ("%d ", a);
  a = ones (2,2) & ones (2,2);
  __printf_assert__ ("%d ", a);
  a = ones (2,2) | zeros (2,2);
  __printf_assert__ ("%d ", a);

endfunction

function out = truthy (i)
  __printf_assert__ ("truthy%d ", i);
  out = 1;
end

function out = falsy (i)
  __printf_assert__ ("falsy%d ", i);
  out = 0;
end
