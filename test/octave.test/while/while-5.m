i = 0;
while (++i < 5)
  if (i < 3)
    continue;
  endif
  printf ("%d", i);
endwhile
printf ("\n");
