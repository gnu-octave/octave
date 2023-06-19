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
end

function a = suby()
  a = {1,2};
end
