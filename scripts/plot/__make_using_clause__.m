function usingstr = __make_using_clause__ (x)
  cols = columns (x);
  if (cols > 0)
    usingstr = strcat (gnuplot_command_using, " ($1)");
    for k = 2:cols
      usingstr = sprintf ("%s:($%d)", usingstr, k);
    endfor
  else
    usingstr = "";
  endif
endfunction
