function display (p)

  fprintf ("%s =", inputname (1));

  a = p.poly;
  first = true;
  for i = 1 : length (a);
    if (a(i) != 0)
      if (first)
        first = false;
      elseif (a(i) > 0 || isnan (a(i)))
        fprintf (" +");
      endif
      if (a(i) < 0)
        fprintf (" -");
      endif
      if (i == 1)
        fprintf (" %.5g", abs (a(i)));
      elseif (abs (a(i)) != 1)
        fprintf (" %.5g *", abs (a(i)));
      endif
      if (i > 1)
        fprintf (" X");
      endif
      if (i > 2)
        fprintf (" ^ %d", i - 1);
      endif
    endif
  endfor

  if (first)
    fprintf (" 0");
  endif
  fprintf ("\n");

endfunction
