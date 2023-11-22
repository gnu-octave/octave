function bytecode_while ()
  i = 5;
  while i
    __printf_assert__ ("%d ", i);
    i--;
  end

  i = 0;
  while i < 3
    i++;
  end
  __printf_assert__ ("%d ", i);

  i = 0;
  ctr = 0;
  while i++ < 4
    ctr++;
  end
  __printf_assert__ ("%d ", i);
  __printf_assert__ ("%d ", ctr);

  i = 0;
  ctr = 0;
  while ++i < 4
    ctr++;
  end
  __printf_assert__ ("%d ", i);
  __printf_assert__ ("%d ", ctr);

  i = 0;
  ctr = 0;
  while i < 4
    i++;
    if i == 2
      continue
    end
    ctr++;
  end
  __printf_assert__ ("%d ", ctr);
  __printf_assert__ ("%d ", i);

  i = 0;
  ctr = 0;
  while i < 4
    i++;
    if i == 2
      break
    end
    ctr++;
  end
  __printf_assert__ ("%d ", ctr);
  __printf_assert__ ("%d ", i);

  i = 0;
  ctr = 0;
  while i < 4
    i++;
    if i == 2
      continue
    elseif i == 3
      break
    end
    ctr++;
  end
  __printf_assert__ ("%d ", ctr);
  __printf_assert__ ("%d ", i);

  i = 0;
  while i < 4
    i++;
    if i == 1
      continue
    else
      break
    end
  end
  __printf_assert__ ("%d ", i);

  ctr = 0;
  j = 0;
  while j < 2
    i = 0;
    while i < 2
      k = 0;
      while k < 2
        k++;
        ctr++;
      end
      i++;
    end
    j++;
  end
  __printf_assert__ ("%d ", ctr);

  i = 0;
  while i++ < 2
    continue
  end
  __printf_assert__ ("%d ", i);

  i = 0;
  while i++ < 2
    break
  end
  __printf_assert__ ("%d ", i);

  i = 0;
  while i++ < 2
  end
  __printf_assert__ ("%d ", i);
end
