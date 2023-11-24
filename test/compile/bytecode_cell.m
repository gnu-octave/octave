function bytecode_cell ()
  a = {'a','b'};
  __printf_assert__ ("%s ", a{1});
  __printf_assert__ ("%s ", a{2});
  __printf_assert__ ("%s ", a{:});
  __printf_assert__ ("%d ", size (a));

  b = 'b';
  c = 'c';

  a = {b, c};
  __printf_assert__ ("%s ", a{1});
  __printf_assert__ ("%s ", a{2});
  __printf_assert__ ("%s ", a{:});
  __printf_assert__ ("%d ", size (a));
  __printf_assert__ ("%s ", class (a{1}));

  d = 'd';
  e = 'e';

  a = {b, c; d, e};
  __printf_assert__ ("%s ", a{1});
  __printf_assert__ ("%s ", a{3});
  __printf_assert__ ("%s ", a{1,2});
  __printf_assert__ ("%s ", a{2,1});
  __printf_assert__ ("%s ", a{:});
  __printf_assert__ ("%d ", size (a));

  f = 'f';
  g = 'g';
  h = 'h';
  i = 'i';
  a = {b, c; d, e; f g; h, i; 'j', 'k'; 'l', 'm'};
  __printf_assert__ ("%s ", a{:});
  __printf_assert__ ("%d ", size (a));

  b = 1;
  c = 2;
  d = 3;
  e = 4;
  a = {b, c; d, e};
  __printf_assert__ ("%d ", a{1});
  __printf_assert__ ("%d ", a{3});
  __printf_assert__ ("%d ", a{1,2});
  __printf_assert__ ("%d ", a{2,1});
  __printf_assert__ ("%d ", a{:});
  __printf_assert__ ("%d ", a{:, 1});
  __printf_assert__ ("%d ", a{1, :});
  __printf_assert__ ("%d ", a{:, :});
  __printf_assert__ ("%d ", size (a));
  __printf_assert__ ("%s ", class (a{1}));

  a = {'qwe','asd','zxc'};
  f = a{:};
  __printf_assert__ ("%s ", f);
  __printf_assert__ ("%d ", size (f));
  __printf_assert__ ("%s ", class (f));

  % Command form function call subref
  __printf_assert__ ("%d ", suby{:});

  % Test making cells dynamically with unpacking of cells
  a = {1,2};
  b = {};
  d = {11; 12};

  c = {a{:}};
  assert (c, {1, 2});

  c = {d{:}};
  assert (c, {11, 12});

  c = {a{:}, 3, 4};
  assert (c, {1, 2, 3, 4});

  c = {b{:}, a{:}, 3, 4, b{:}};
  assert (c, {1, 2, 3, 4});

  c = {;;; a{:}; 3 4;;;; b{:}};
  assert (c, {1, 2; 3, 4});

  c = {b{:}};
  assert (c, {});

  c = {b{:}; b{:}};
  assert (c, cell (2, 0));

  threw = false;
  try
    c = {b{:}; 1 2};
  catch e
    assert (regexp (e.message, "number of columns must match"))
    threw = true;
  end

  assert (threw)

  threw = false;
  try
    c = {1 2 3; a{:}};
  catch e
    assert (regexp (e.message, "number of columns must match"))
    threw = true;
  end

  assert (threw)

  threw = false;
  try
    c = {1 2 3; a{:}; 4 5 6};
  catch e
    assert (regexp (e.message, "number of columns must match"))
    threw = true;
  end

  assert (threw)

  threw = false;
  try
    c = {a{:}; 1 2 3};
  catch e
    assert (regexp (e.message, "number of columns must match"))
    threw = true;
  end

  assert (threw)

end

function a = suby()
  a = {1,2};
end
