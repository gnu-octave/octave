function r = colon (a, b, c)
  if (nargin == 2)
    r = sprintf ("%s:%s", class (a), class (b));
  else
    r = sprintf ("%s:%s:%s", class (a), class (b), class (c));
  endif
endfunction
