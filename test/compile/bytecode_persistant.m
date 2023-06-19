function bytecode_persistant ()
  q = 3;
  l = 4;
  
  persistent a = 3;
  __printf_assert__ ("a:%d ", a++);

  persistent b;
  __printf_assert__ ("b:%d ", b);
  __printf_assert__ ("%s ", class (b));
  __printf_assert__ ("%d ", size (b));
  b = 0;
  __printf_assert__ ("%d ", b++);

  suby ();
  suby ();
end

function suby ()
  persistent c = 2;
  c++;
  __printf_assert__ ("c:%d ", c);
end
