% TODO: The anonymous functions bodies are not compiled

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
  __printf_assert__ ("%d ", h3 (1, 2, 1));

  h3 (1, 2, 1);
  __printf_assert__ ("%d ", ans);

  h4 = @() {1,2,3}{:};
  [a b c] = h4();
  __printf_assert__ ("%d %d %d ", a, b, c);
  [a b c] = h4();
  __printf_assert__ ("%d %d %d ", a, b, c);
  [a b] = h4();
  __printf_assert__ ("%d %d ", a, b);
  [a b] = h4();
  __printf_assert__ ("%d %d ", a, b);

  h5 = @(x) @(y) __printf_assert__ ("%d %d ", x, y);
  h5(11)(12)
  h5(11)(12)

  % max not in parent scope
  h6 = @(x, y) max (x, y);
  __printf_assert__ ("%d ", h6 (-1, 1));

  % Mess with the anon function's stackframe
  a = 3;
  h7 = @() foo () + a;
  __printf_assert__ ("%d ", h7 ()); % 4
  __printf_assert__ ("%d ", h7 ()); % also 4

  % Nargout
  h8 = @() expression_nargout ();
  a = h8 ();
  __printf_assert__ ("%d ", a);
  a = h8 ();
  __printf_assert__ ("%d ", a);

  [a b] = h8 ();
  __printf_assert__ ("%d ", a, b);
  [a b] = h8 ();
  __printf_assert__ ("%d ", a, b);

  [a b c] = h8 ();
  __printf_assert__ ("%d ", a, b, c);
  [a b c] = h8 ();
  __printf_assert__ ("%d ", a, b, c);

  [a, ~, c] = h8 ();
  __printf_assert__ ("%d ", a, c);
  h8 = @() expression_nargout ();
  [a, ~, c] = h8 ();
  __printf_assert__ ("%d ", a, c);

  % ans
  h9 = @() 9;
  h9 ();
   __printf_assert__ ("%d ", ans);

  % word command
  h10 = @() nargout;
  h10 ();
  __printf_assert__ ("%d ", ans);
  a = h10 ();
  __printf_assert__ ("%d ", a);

  % inputname
  h11 = @(x) inputname (1);
  fooo = 123;
  __printf_assert__ ("%s ", h11 (fooo));

  % Ignored outputs are propagated to nested calls
  h12 = @() try_isargout ();
  x = y = z = 0;
  [x, ~, z] = h12 (); __printf_assert__ ("%d%d%d ", x, y, z);
  [x, ~, z] = h12 (); __printf_assert__ ("%d%d%d ", x, y, z);
  [x, y, ~] = h12 (); __printf_assert__ ("%d%d%d ", x, y, z);
  [x, ~, z] = h12 (); __printf_assert__ ("%d%d%d ", x, y, z);
  [~, y, ~] = h12 (); __printf_assert__ ("%d%d%d ", x, y, z);
  [~, ~, ~] = h12 (); __printf_assert__ ("%d%d%d ", x, y, z);
  [~, y, z] = h12 (); __printf_assert__ ("%d%d%d ", x, y, z);

  % The optim package exposed a bug with EXPAND_CS_LIST during development
  h1 = @ (p) - (p(1)^2 + 1 - p(2));
  h2 = @ (p) {[], h1(p)}{:};
  [~, a] = h2 ([-2 5]);
  assert (a == 0)
  [~, a] = h2 ([-2 5]);
  assert (a == 0)

  % Nested anon functions
  h1 = @(y) y * 2;
  h2 = @(yy) execute_handle (@(yyy) h1 (yyy), yy); %h1 captured here
  assert (h2 (3) == 6)
  h2 = @(yy) execute_handle (@(yyyy) execute_handle (@(yyy) h1 (yyy), yyyy), yy); % Nest some more
  assert (h2 (3) == 6)
end

function [x, y, z] = try_isargout ()
  __printf_assert__ ("~%d%d%d ", isargout (1), isargout (2), isargout (3))
  x = 1; y = 2; z = 3;

  bar (); % Does nothing. Check return from nested subfunction works with active ignore
endfunction

function bar
end

function ret = foo
  evalin ("caller", "a++;"); % Should not change 'a' for the next time h7 is called
  ret = 0;
end

function varargout = expression_nargout ()
  varargout = cell (1, nargout);
  for i = 1:nargout
    varargout{i} = i;
  end
end

function b = execute_handle (h, arg1)
  b = h (arg1);
end
