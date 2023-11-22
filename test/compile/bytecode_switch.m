function bytecode_switch ()
  a = 2;
  switch (a)
    case 1
      __printf_assert__ ("boo ");
    case 2
      __printf_assert__ ("yay ");
    otherwise
      __printf_assert__ ("boo ");
  end

  switch (a)
    case 1
      __printf_assert__ ("boo ");
    case 3
      __printf_assert__ ("boo ");
    otherwise
      __printf_assert__ ("yay2 ");
  end

  b = "yay3 ";
  switch (b)
    case "boo1"
      __printf_assert__ ("boo ");
    case "yay3 "
      __printf_assert__ ("yay3 ");
    otherwise
      __printf_assert__ ("boo ");
  end

  % Cells
  a = 2;
  switch (a)
    case {1,0}
      __printf_assert__ ("boo ");
    case {2,3}
      __printf_assert__ ("yay4 ");
    otherwise
      __printf_assert__ ("boo ");
  end

  switch (a)
    case {1,0}
      __printf_assert__ ("boo ");
    case {3,2} %Swapped
      __printf_assert__ ("yay5 ");
    otherwise
      __printf_assert__ ("boo ");
  end

  switch (a)
    case {1,0}
      __printf_assert__ ("boo ");
    case {3,4}
      __printf_assert__ ("boo ");
    otherwise
      __printf_assert__ ("yay6 ");
  end

  % Silly
  switch (a)
    otherwise
      __printf_assert__ ("yay7 ");
  end

  % Empty
  switch (a)
  end

  % No default case
  a = 2;
  switch (a)
    case 1
      __printf_assert__ ("boo ");
    case 2
      __printf_assert__ ("yay8 ");
  end

  switch (a)
    case 1
      __printf_assert__ ("boo ");
    case 3
      __printf_assert__ ("boo ");
  end

  % Return from switch
  __printf_assert__ ("%d ", returnfromswitch (1));
  __printf_assert__ ("%d ", returnfromswitch (2));
  __printf_assert__ ("%d ", returnfromswitch (3));
  __printf_assert__ ("%d ", returnfromswitch2 (1));
  __printf_assert__ ("%d ", returnfromswitch2 (2));
  __printf_assert__ ("%d ", returnfromswitch2 (3));

  % switch with continue

  for i = 1:4
    switch (i)
      case 1
        __printf_assert__ ("1:%d ", i);
      case 2
        __printf_assert__ ("2:%d ", i);
        continue;
      case 3
        __printf_assert__ ("3:%d ", i);
      otherwise
        __printf_assert__ ("breaking:%d ", i);
        break;
    endswitch
    __printf_assert__ ("for-end:%d", i);
  end

end


function a = returnfromswitch (b)
  switch (b)
    case 1
      a = 1;
      return
    case 2
      a = 2;
      return;
    otherwise
      a = 3;
      return
  end

  __printf_assert__ ("boo ");
end

function a = returnfromswitch2 (b)
  % switches and fors cleans the stack at returns
  % in a special way so test that that works properly
  %
  % The for loops puts native ints on the stack, so we can't just pop
  % the stack assuming everything is octave values.
  %
  for i = [1, 2] % Puts int n,int i and the range on the stack
    switch (b) % Puts b on the stack
      case 10
        return
      otherwise
        for j = [3, 4] % Puts int n,int i and the range on the stack
          __printf_assert__ ("%d ", j);
          switch (b) % Puts b on the stack
            case 1
              a = 1;
              return %pop, popint popint pop, pop, popint popint pop
            case 2
              a = 2;
            otherwise
              a = 3;
              return
          end

          __printf_assert__ ("%d ", j);
        end
    end

    __printf_assert__ ("%d ", i);
  end

  __printf_assert__ ("yoo ");
end
