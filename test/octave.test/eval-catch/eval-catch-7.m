function ms = mangle (s)
  ## Wrap angle brackets around S.
  ms = strcat ("<", s, ">");
endfunction
eval ("clear a; a;", "mangle (__error_text__)");
