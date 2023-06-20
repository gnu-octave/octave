function bytecode_eval_1 ()
  % Simple
  assert (2 == eval ("2"));
  assert (2 == eval ("2;"));

  v = eval("11");
  assert (v == 11);

  % ans
  eval ("12;");
  assert (ans == 12);

  % Change variable value
  a = 2;
  eval ("a = 3;");

  __printf_assert__ ("%f ", a)
  __printf_assert__ ("%f ", size (a))
  __printf_assert__ ("%s ", class (a));

  % Create new variable in an eval
  eval ("b = 4;");
  __printf_assert__ ("%f ", b)
  __printf_assert__ ("%f ", size (b))
  __printf_assert__ ("%s ", class (b));

  % Create new variable in an eval, that is also not
  % not in a bytecode slot
  eval ("c = 4;");
  __printf_assert__ ("%f ", eval("c"))
  __printf_assert__ ("%f ", size (eval("c")))
  __printf_assert__ ("%s ", class (eval("c")));
  eval ("c = 5;");
  __printf_assert__ ("%f ", eval("c"))

  % Change a global in an eval
  clear global d
  global d = 3;
  eval ("d = 4;")
  __printf_assert__ ("%f ", d);
  clear global d
  d = 2;
  __printf_assert__ ("%f ", d);

  % Create a global in an eval

  %% TODO: Not supported. Does it have to be?
  % eval ("clear global e");
  % eval ("global e = 5;")
  % __printf_assert__ ("%f ", e);
  % __printf_assert__ ("%d ", length(who('global','e')));

  % Just test the same thing in a subfunction
  sub1 ();
  
  % Change the value of arguments and returns in an eval
  % Also do nargin and nargout in a subfunction
  [aa bb] = suby2 (11, 22, 33);
  __printf_assert__ ("%f ", aa);
  __printf_assert__ ("%f ", bb);
end

function sub1()
  % Simple
  assert (2 == eval ("2;"));
  assert (2 == eval ("2;"));

  v = eval("11;");
  assert (v == 11);

  % ans
  eval ("12;");
  assert (ans == 12);

  % Change variable value
  a = 2;
  eval ("a = 3;");

  __printf_assert__ ("%f ", a)
  __printf_assert__ ("%f ", size (a))
  __printf_assert__ ("%s ", class (a));

  % Create new variable in an eval
  eval ("b = 4;");
  __printf_assert__ ("%f ", b)
  __printf_assert__ ("%f ", size (b))
  __printf_assert__ ("%s ", class (b));

  % Create new variable in an eval, that is also not
  % not in a bytecode slot
  eval ("c = 4;");
  __printf_assert__ ("%f ", eval("c"))
  __printf_assert__ ("%f ", size (eval("c")))
  __printf_assert__ ("%s ", class (eval("c")));
  eval ("c = 5;");
  __printf_assert__ ("%f ", eval("c"))

  % Change a global in an eval
  clear global d
  global d = 3;
  eval ("d = 4;")
  __printf_assert__ ("%f ", d);
  clear global d
  d = 2;
  __printf_assert__ ("%f ", d);
end

function [c d] = suby2 (a, b, c)
  __printf_assert__ ("1:%f ", a);
  __printf_assert__ ("2:%f ", b);
  __printf_assert__ ("3:%f ", c);

  eval ("c = 3;")
  eval ("a = c;")
  eval ("d = a;")
  __printf_assert__ ("4:%f ", a);
  __printf_assert__ ("5:%f ", b);
  __printf_assert__ ("6:%f ", c);
  __printf_assert__ ("7:%f ", d);
  
  __printf_assert__ ("%d ", nargin);
  __printf_assert__ ("%d ", eval ("nargin"));

  __printf_assert__ ("%d ", nargout);
  __printf_assert__ ("%d ", eval ("nargout"));
end