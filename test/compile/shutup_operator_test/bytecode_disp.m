function bytecode_disp()
  %  int8 type's display is overloaded with a __printf_assert__
  1               % "ans = 1"
  print_dot;
  2 + 3           % "ans = 5"
  print_dot;
  0;              %
  0 + 0;          %
  print_dot;
  ~4              % "ans = 0"
  print_dot;
  2^3             % "ans = 8"
  print_dot       %

  if 2
    3             % "ans = 3"
  end
  print_dot;
  x = 4 - 1       % "x = 3"

  print_dot;
  [x, y] = deal (1,2) % "x = 1" "y = 2"
  print_dot;
  [x, ~] = deal (1,2) % "x = 1"
  print_dot;
  % If all lvalues are black holes, nothing is printed
  [~, ~] = deal (1,2) %
  print_dot;
  [x, ~, ~] = deal (1,2, 3) % "x = 1"
  print_dot;
  [~, y, ~] = deal (1,2, 3) % "y = 2"
  print_dot;
  x
end

function print_dot()
  __printf_assert__(". ");
end
