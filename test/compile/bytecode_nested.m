function bytecode_nested ()

  %% Empty function
  a = 1; b = 1; c = 1;
  function nested1
  end

  nested1
  nested1 ();

  %% Changes value of a
  function nested2 ()
    assert (a == 1);
    a = 111;
    assert (a == 111);
  end

  nested2 ();
  assert (a == 111);

  %% Changes value of b
  function nested3 (a)
    assert (a == 2);
    a = 222; % Does not update a in parent
    b = 111;
    assert (a == 222);
  end

  a = 1; b = 1; c = 1;
  nested3 (2);
  assert (a == 1); % a still same, since argument in nested3
  assert (b == 111);

  %% a, b and c are arguments and return values in nested4
  %% and do not change in the outer frame
  function c = nested4 (a, b)
    assert (a == 3)
    assert (b == 4);
    a = 0;
    b = 0;
    c = 2;
  end

  a = 1; b = 1; c = 1;
  assert (nested4 (3, 4) == 2);
  assert (a == 1);
  assert (b == 1);
  assert (c == 1);

  %% Sets local variable that should not leak to this frame
  a = 1; b = 1; c = 1;
  nested5;
  assert (! exist ('d5'))

  function nested5
    d5 = 3;
    assert (d5 == 3);
  end

  %% Calls empty nested function nested61
  function nested6
    function nested61
    end
  end

  nested6;
  nested6 ();

  %% Nested function nested71
  function nested7
    a = 2;
    d7 = 2;
    function nested71
      assert (a == 2)
      assert (d7 == 2)
      a = 3;
      d7 = 3;
    end

    nested71;
    assert (a == 3)
    assert (d7 == 3)
  end

  a = 1; b = 1; c = 1;
  nested7;
  assert (a == 3)

  %% Nested with args
  function b = nested8(a)
    assert (a == 2)
    b = 2;

    % Args and returns are not shared
    function b = nested81(a)
      assert (a == 3)
      b = 3;
      assert (b == 3)
    end

    nested81 (3);
    assert (a == 2)
    assert (b == 2)
  end

  a = 1; b = 1; c = 1;
  nested8 (2);
  assert (a == 1)
  assert (b == 1)

  %% Recursive nested function
  function b = nested9 (a)
    d9 = 0; % d9 is not shared with recursive calls
    assert (c == a + 1)
    c = a;

    a_cpy = a; % Test that argument are not changed by children

    if a == 0
      b = 3;
      d9 = 3; % d9 does not change in parent frames
      return;
    end

    b = 0;

    b_tmp = nested9 (a - 1);
    assert (b == 0)
    b = b_tmp;

    assert (d9 == 0)
    assert (b == 3)
    assert (a == a_cpy)
  end

  a = 1; b = 1;
  c = 9;
  ret = nested9 (8);
  assert (a == 1)
  assert (b == 1)
  assert (c == 0)
  assert (ret == 3)

  %% Call siblings
  function b = nested10 (d10, e10=true)
    a = 1; b = 1; c = 1;
    nested7;
    assert (a == 3)

    a = 1; b = 1;
    c = 9;
    ret = nested9 (8);
    assert (a == 1)
    assert (b == 1)
    assert (c == 0)
    assert (ret == 3)

    if ! d10
      return;
    end

    nested10 (d10 - 1, e10); % Test siblings from recursive call

    if e10
      nested11 (); % Calls nested10
    end
  end

  function nested11
    nested10(2, false);

    a = 1; b = 1; c = 1;
    assert (nested4 (3, 4) == 2);
    assert (a == 1);
    assert (b == 1);
    assert (c == 1);
  end

  a = 1; b = 1; c = 1;
  nested10 (1);

  %% Test globals

  function nested12
    assert (isglobal ("glb_d"))
    % Note: If a global is not added to the frame of nested12,
    % is is not marked as global.
    assert (!isglobal ("glb_e")) % Not a global, since not added to nested12's frame
    assert (glb_d == 3)
    glb_d = 4;

    eval ("glb_f = 24;"); % glb_f on dynamic frame in nested12

    function nested12_1
      assert (isglobal ("glb_d"))
      assert (isglobal ("glb_e"))
      assert (glb_d == 4)
      assert (glb_e == 13)
      eval ("assert (glb_f == 24)");
      glb_d = 5;
      glb_e = 14;
    end

    nested12_1;

    nested_sibling13;
  end

  function nested_sibling13
    assert (!isglobal ("glb_d"))
    assert (isglobal ("glb_e"))
    assert (glb_e == 14)
    glb_e = 15;
  end

  global glb_d;
  global glb_e;
  global glb_f;

  glb_d = 3;
  glb_e = 13;
  glb_f = 23;

  nested12;
  assert (glb_d == 5)
  assert (glb_e == 15)
  assert (glb_f == 24)

  %% Can't add dynamic variables
  function nested14
    eval ("a14 = 3;")
  end

  try
    nested14;
  catch e
    assert (regexp (e.message, "can not add variable"));
  end

  try
    eval ("aaa = 3;") % Can't add dynamic variable in root either
  catch e
    assert (regexp (e.message, "can not add variable"));
  end

  %% evalin
  a = 1; b = 1; c = 1;
  function nested15
    evalin ("caller", "assert (b == 3);");
    a15 = 15;

    function nested15_1 (a15)
      evalin ("caller", "assert (a15 == 15);");
    end
    nested15_1 (10);
  end

  b = 3;
  nested15;

  %% nargout, isargout
  a = 1; b = 1; c = 1;
  function [a, b, c] = nested16 ()
    function [a, b, c] = nested16_1 ()
      a = nargout;
      b = 2;
      c = 3;
    end
    a = nargout;
    b = 2;
    c = 3;

    glb_d = [isargout(1), isargout(2), isargout(3)];

    [a2, b2, c2] = nested16_1 ();
    assert (a2 == 3);
    [a2, b2] = nested16_1 ();
    assert (a2 == 2);
    [a2] = nested16_1 ();
    assert (a2 == 1);
  end

  [a, b, c] = nested16 ();
  assert (a == 3);
  assert (glb_d == [1 1 1]) % isargout stored in glb_d
  [a, b] = nested16 ();
  assert (a == 2);
  assert (glb_d == [1 1 0])
  [a] = nested16 ();
  assert (a == 1);
  assert (glb_d == [1 0 0])
  [~] = nested16 ();
  assert (glb_d == [0 0 0])
  [~, b] = nested16 ();
  assert (b == 2)
  assert (glb_d == [0 1 0])
  [~, b, ~] = nested16 ();
  assert (b == 2)
  assert (glb_d == [0 1 0])
  [~, b, c] = nested16 ();
  assert (b == 2)
  assert (c == 3)
  assert (glb_d == [0 1 1])

  %% Call handle to nested function 1
  function ret = nested17
    ret = b;
  end

  b = 3;

  h1 = @nested17;

  assert (call_handle1 (h1) == 3);
  assert (h1 () == 3)
  b = 4;
  assert (call_handle1 (h1) == 4);

  %% Changes value in decoupled nested frame
  h2 = sub_returns_nested_fn;
  h3 = sub_returns_nested_fn;

  assert (h2() == 33)
  assert (h2() == 34)
  assert (call_handle1(h2) == 35)
  assert (h3() == 33)

  c1 = cdef_bar ("1");

  % Two levels of nested nesting
  h4 = sub_returns_nested_fn2;
  assert (h4 () == 1)
  assert (h4 () == 2)
end

function subby
end

function a = call_handle1 (h)
  a = h ();
end

function h = sub_returns_nested_fn ()
  a = 33;
  c2 = cdef_bar ("2");
  function ret = sub_nested1
    ret = a;
    a++;
    c3 = cdef_bar ("3");
  end

  h = @sub_nested1;
end

function h1 = sub_returns_nested_fn2
  c2 = cdef_bar ("4");

  function h2 = nested_fn1
    a = 1;
    c3 = cdef_bar ("5");

    function ret = nested_fn2
      c4 = cdef_bar ("6");
      ret = a;
      a++;
    end

    h2 = @nested_fn2;
  end

  h1 = nested_fn1 ();
end