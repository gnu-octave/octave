function bytecode_struct ()
  s = struct ('a', 1, 'b', 2);
  __printf_assert__ ("%d ", s.a);
  __printf_assert__ ("%d ", s.b);
  __printf_assert__ ("%s ", class (s.a));
  __printf_assert__ ("%d ", size (s.a));

  % Should not mess up stack
  s.a;

  % Test simple assigns
  r.a = 3;

  __printf_assert__ ("%s ", class (r));
  __printf_assert__ ("%d ", r.a);

  % Test word command struct subref

  __printf_assert__ ("%d ", suby.b);

  % Bug 64817
  test_bug_64817;
end

function a = suby ()
  a.b = 4;
end

function test_bug_64817
  % Field id slot collided with variable slot when making
  % the script frame.

  script_defines_qweqwe; % Just does "qweqwe = 0;"

  if 0
    asd.qweqwe; % 'qweqwe' is a field here
  end
end
