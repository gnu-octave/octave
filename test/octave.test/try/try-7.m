function ms = mangle (s)
  ## Wrap angle brackets around S.
  ms = strcat ("<", s, ">");
endfunction
try
  clear a
  a;
catch
  mangle (lasterr)
end_try_catch
