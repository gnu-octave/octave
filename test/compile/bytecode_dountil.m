function bytecode_dountil ()
  i = 5;
  do
    __printf_assert__ ("%d ", i);
    i--;
  until i

  i = 0;
  do
    i++;
  until i >= 3
  __printf_assert__ ("%d ", i);

  i = 0;
  ctr = 0;
  do
    ctr++;
  until i++ >= 4
  __printf_assert__ ("%d ", i);
  __printf_assert__ ("%d ", ctr);

  i = 0;
  ctr = 0;
  do
    ctr++;
  until ++i >= 4
  __printf_assert__ ("%d ", i);
  __printf_assert__ ("%d ", ctr);

  i = 0;
  ctr = 0;
  do
    i++;
    if i == 2
      continue
    end
    ctr++;
  until i >= 4
  __printf_assert__ ("%d ", ctr);
  __printf_assert__ ("%d ", i);

  i = 0;
  ctr = 0;
  do
    i++;
    if i == 2
      break
    end
    ctr++;
  until i >= 4
  __printf_assert__ ("%d ", ctr);
  __printf_assert__ ("%d ", i);

  i = 0;
  ctr = 0;
  do
    i++;
    if i == 2
      continue
    elseif i == 3
      break
    end
    ctr++;
  until i >= 4
  __printf_assert__ ("%d ", ctr);
  __printf_assert__ ("%d ", i);

  i = 0;
  do
    i++;
    if i == 1
      continue
    else
      break
    end
  until i > 100
  __printf_assert__ ("%d ", i);

  ctr = 0;
  j = 0;
  do
    i = 0;
    do
      k = 0;
      do
        k++;
        ctr++;
      until k >= 2
      i++;
    until i > 2
    j++;
  until j >= 2
  __printf_assert__ ("%d ", ctr);

  i = 0;
  do
    i++;
    if i == 4
      break;
    end
    continue;
  until i == 3
  __printf_assert__ ("%d ", i);

  i = 0;
  do
    break
  until i++ > 2
  __printf_assert__ ("%d ", i);

  i = 0;
  do
  until i++ == 2
  __printf_assert__ ("%d ", i);
end
