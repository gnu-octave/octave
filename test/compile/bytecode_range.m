function bytecode_range ()

  % The should be range constants, but I think
  % negative limits are not folded to range constants

  a = 1:3;
  __printf_assert__ ("%d ", a);

  a = 1:2:6;
  __printf_assert__ ("%d ", a);

  a = 1:2:5;
  __printf_assert__ ("%d ", a);

  a = 1:0.1:1.4;
  __printf_assert__ ("%d ", a);

  a = 1:-0.1:0.7;
  __printf_assert__ ("%d ", a);

  a = 7:7;
  __printf_assert__ ("%d ", a);

  a = 7:-1:7;
  __printf_assert__ ("%d ", a);

  a = 7:-1:8;
  __printf_assert__ ("%d ", isempty (a));

  % Dynamically created with COLON2 or 3 opcode.
  % Colons behave differently when in command expression,
  % they use the COLONX_CMD opcodes.
  %
  % ??? I don't think the ranges are allocated as matrixes
  % when used in commands.
  base = 8;
  inc = 2;
  lim = 11;

  a = base : inc : lim;
  __printf_assert__ ("%d ", a);
  for i = base : inc : lim
    __printf_assert__ ("%d ", i);
  end

  a = base : lim;
  __printf_assert__ ("%d ", a);
  for i = base : lim
    __printf_assert__ ("%d ", i);
  end

  base = 10;
  inc = -2;
  lim = 7;

  a = base : inc : lim;
  __printf_assert__ ("%d ", a);
  for i = base : inc : lim
    __printf_assert__ ("%d ", i);
  end

  a = -base : -lim;
  __printf_assert__ ("%d ", a);
  for i = -base : -lim
    __printf_assert__ ("%d ", i);
  end

end
