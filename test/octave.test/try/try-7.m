function ms = mangle (s)
  ## Wrap angle brackets around S.
  ms = strcat ("<", s, ">");
endfunction
try
  clear a
  a;
catch
  mangle (__error_text__)
end_try_catch
