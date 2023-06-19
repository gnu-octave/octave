function bytecode_return ()
  a = foo ();
  __printf_assert__ ("%d ", a);

  bar (1);
  bar (0);
  baz (0);
  baz (1);
  baz (2);

  boz();

  meh ();

  a = return_1 ();
  __printf_assert__ ("%d ", a);
  [a b] = return_2 ();
  __printf_assert__ ("%d ", a);
  __printf_assert__ ("%d ", b);

  % Drop one output variable
  a = return_2 ();
  __printf_assert__ ("%d ", a);

  % Drop all output variables
  return_2 ();


  % Command form call
  a = return_1;
  __printf_assert__ ("%d ", a);

  [a b] = return_2;
  __printf_assert__ ("%d ", a);
  __printf_assert__ ("%d ", b);

  a = return_2;
  __printf_assert__ ("%d ", a);

  return_2;

  silly();
  silly(2);
end

function [a b] = silly(i)
  __printf_assert__ ("silly ");
end

function a = return_1 ()
  a = 1;
end

function [a b] = return_2 ()
  a = 1;
  b = 2;
end

function out = foo ()
  out = 2;
  return
end

function bar (i)
  if i
    __printf_assert__ ("baaar ");
    return
  end

  __printf_assert__ ("bääär ");
end

function out = baz (i)
  if i == 0
    __printf_assert__ ("baaaaz ");
    return
  else i == 1
    __printf_assert__ ("bääääz ");
    return
  end

  __printf_assert__ ("bååååz ");
end

function boz ()
  __printf_assert__ ("booz ");
  return
  __printf_assert__ ("booo ");
end

function meh ()
  return
end
