function bytecode_multi_assign ()
  A = [1 2; 3 4];
  [a, b] = max (A);
  __printf_assert__ ("%d ", a);
  __printf_assert__ ("%d ", b);

  [a,b,c,d] = foo ();
  __printf_assert__ ("%d ", a);
  __printf_assert__ ("%d ", b);
  __printf_assert__ ("%d ", c);
  __printf_assert__ ("%d ", d);

  % Non ids in lhs
  % Eval is used as a cheat since
  % rhs need to know how many lhs values
  % there are.

  [e, f.a, g, h.b] = foo ();
  __printf_assert__ ("%d ", e);
  __printf_assert__ ("%d ", f.a);
  __printf_assert__ ("%d ", g);
  __printf_assert__ ("%d ", h.b);

  e = [1 2 3];
  g = {1, 2, 3};
  [e(2), f.a, g{2}, h.b] = foo ();
  __printf_assert__ ("%d ", e);
  __printf_assert__ ("%d ", f.a);
  __printf_assert__ ("%d ", g{2});
  __printf_assert__ ("%d ", h.b);

  [e(end), f.a, g{min (100, end)}, h.b] = foo ();
  __printf_assert__ ("%d ", e);
  __printf_assert__ ("%d ", f.a);
  __printf_assert__ ("%d ", g{min (100, end)});
  __printf_assert__ ("%d ", h.b);

  [e(end), f.a, ~, h.b] = foo ();
  __printf_assert__ ("%d ", e);
  __printf_assert__ ("%d ", f.a);
  __printf_assert__ ("%d ", g{end});
  __printf_assert__ ("%d ", h.b);


  [C{1:2}, D] = {1,2,3}{:};
  __printf_assert__ ("%d ", C{1});
  __printf_assert__ ("%d ", C{2});
  __printf_assert__ ("%d ", D);

  % Check that opcodes SET_IGNORE_OUTPUTS and CLEAR_IGNORE_OUTPUTS
  % does not mess up if a nested expression throws before SET_IGNORE_OUTPUTS
  % is executed.
  try
    [~, b] = bar (baz_throws ());
  catch
  end
end

function [a,b,c,d] = foo ()
  a = 1;
  b = 2;
  c = 3;
  d = 4;
end

function [a b] = bar (c)
  a = 1;
  b = 0;
end

function a = baz_throws ()
  error ("qwe");
end
