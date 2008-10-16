function p = polynomial (a)
  if (nargin == 0)
    p.poly = [];
    p = class (p, "polynomial");
  elseif (nargin == 1)
    if (strcmp (class (a), "polynomial"))
      p = a;
    elseif (isvector (a) && isreal (a))
      p.poly = a(:)';
      p = class (p, "polynomial");
    else
      error ("polynomial: expecting real or complex vector")
    endif
  else
    print_usage ();
  endif
  superiorto ("double");
endfunction
