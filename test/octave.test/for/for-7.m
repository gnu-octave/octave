for i = [1,2;3,4]*I
  printf ("%d", imag (i(1,1)));
  printf ("%d", imag (i(2,1)));
endfor
printf ("\n");
