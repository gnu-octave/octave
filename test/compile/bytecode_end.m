function bytecode_end ()

  A = [1 2; 3 4];
  __printf_assert__ ("%d ", A(1:end));

  A(2:end) = [5 6 7];
  __printf_assert__ ("%d ", A);
  __printf_assert__ ("%d ", size (A));

  A = [1 2; 3 4];
  A(end, end) = 5;
  __printf_assert__ ("%d ", A(2, 2));
  __printf_assert__ ("%d ", A(end, end));

  A = [1 2; 3 4];
  A(end - 1, end - 1) = 6;
  __printf_assert__ ("%d ", A(1, 1));
  __printf_assert__ ("%d ", A(end - 1, end - 1));

  A = [1:4];
  A(end + 1) = 5;
  __printf_assert__ ("%d ", A);

  A = 1;
  A(end) = 2;
  __printf_assert__ ("%d ", A);
  __printf_assert__ ("%d ", A(end));

  A(end + 1) = 3;
  __printf_assert__ ("%d ", A);
  __printf_assert__ ("%d ", A(end));

  __printf_assert__ ("%d ", suby1()(end));

  % End indexing an object that is not an id
  s = {"ifs"};
  a = s{1}(2:end);
  __printf_assert__ ("%s ", a);

  % Nested index expressions
  M = [1 2 3 4];
  __printf_assert__ ("%d ", M (min (2, end))); % End of M 
  __printf_assert__ ("%d ", M (max (3, min (2, end)))); % End of M 
  
  min_h = @min;
  __printf_assert__ ("%d ", M (min_h (2, end))); % End of min_h

  s = [struct struct struct];
  s(2).name = "foo";
  __printf_assert__ ("%s ", s(min (2, end)).name);

  % end together with struct refs are annoying
  __printf_assert__ ("%s ", s(2).name (end - 1: end));
end

function a = suby1()
  a = [1 2 3 4];
end