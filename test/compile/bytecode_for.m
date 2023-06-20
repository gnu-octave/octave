function bytecode_for()
  ctr = 0;

  for i = 1:4
    __printf_assert__ ("%d ", i);
  end
  __printf_assert__ ("%d ", i);

  for i = 1:2:5
    __printf_assert__ ("%d ", i);
  end
  __printf_assert__ ("%d ", i);

  for i = 4:-1:1
    __printf_assert__ ("%d ", i);
  end
  __printf_assert__ ("%d ", i);

  for j = 1:4
    break
  end
  __printf_assert__ ("%d ", j);

  for j = 1:4
    continue
  end
  __printf_assert__ ("%d ", j);

  for j = 1:4
    if j == 2
      break
    end
  end
  __printf_assert__ ("%d ", j);

  for j = 1:4
    if j == 2
      break
    else
      continue
    end
  end
  __printf_assert__ ("%d ", j);

  ctr = 0;
  for i = 1:4
    for j = 1:4
      ctr++;
    end
  end
  __printf_assert__ ("%d ", ctr);

  ctr = 0;
  for i = 1:4
    if i == 2
      continue
    end

    for j = 1:4
      if j == 2
        continue
      end

      ctr++;

      if j == 3
        break
      end
    end

    if i == 3
      break
    end
  end
  __printf_assert__ ("%d ", ctr);
  __printf_assert__ ("%d ", i);
  __printf_assert__ ("%d ", j);

  ctr = 0;
  for i = 1:2
    for j = 1:2
      for k = 1:2
        for l = 1:2
          for m = 1:2
            for n = 1:2
              for o = 1:2
                for p = 1:2
                  ctr++;
                end
              end
            end
          end
        end
      end
    end
  end
  __printf_assert__ ("%d ", ctr);

  for i = 1:3
  end
  __printf_assert__ ("%d ", i);

  % sclar range, only executed once
  for i = 2
    __printf_assert__ ("%d ", i);
  end

  n = 1;
  for i = 1:n
    __printf_assert__ ("%d ", i);
  end

  n = 1;
  for i = 2:n
    __printf_assert__ ("boo");
  end
  __printf_assert__ ("%d ", i);
  __printf_assert__ ("%s ", class (i));

  %Matrix
  M = [1 2; 3 4];
  for i = M
    __printf_assert__ ("%d ", i);
    __printf_assert__ ("size %d ", size (i));
  end

  for i = []
    __printf_assert__ ("boo");
  end
  __printf_assert__ ("%d ", i);
  __printf_assert__ ("%s ", class (i));


  n = 'qwe';
  for i = n
    __printf_assert__ ("%s ", i);
    __printf_assert__ ("size %d ", size (i));
  end
  __printf_assert__ ("%s ", class (i));

  % The iteration variable is a double
  % ??? Changed in someones patch? TODO: Bug?
  for i = single(1):single(3)
    if i == 1
      __printf_assert__ ("%s ", class (i));
    end
  end

  % ... unless rhs is a scalar ...
  for i = single (1)
  end
  __printf_assert__ ("%s ", class (i));

  % Test return from for loop (need to pop native integers from stack)
  __printf_assert__ ("%d ", foo ());

  % Iterate over struct
  for s = struct ("a", {"1", "2"}, "b", {"11", "12"})
    __printf_assert__ ("%s %s ", s.a, s.b);
  end

  % Complex for loop

  x.a = 1;
  x.b = [1, 2; 3, 4];
  x.c = "string";
  for [val, key] = x
    __printf_assert__ ("key:%s ", key)
    if isa(val, "char")
      __printf_assert__ ("val:%s ", val)
    else
      __printf_assert__ ("val:%d %d", val, size(val))
    end
  endfor

  for [val, key] = struct ()
    __printf_assert__ ("boo");
  end
end


function i = foo ()
  for i = 1:10
    if i == 5
      return
    end
  end
end
