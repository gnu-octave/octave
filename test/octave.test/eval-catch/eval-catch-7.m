function ms = mangle (s)
  ## Wrap angle brackets around S.
  ms = strcat ("<", s, ">");
endfunction
eval ("clear a; a;", "mangle (lasterr)");
