function s = get (p, f)
  if (nargin == 1)
    s.poly = p.poly;
  elseif (nargin == 2)
    if (ischar (f))
      switch (f)
        case "poly"
          s = p.poly;
        otherwise
          error ("@polynomial/get: invalid property %s", f);
      endswitch
    else
      error ("@polynomial/get: expecting the property to be a string");
    endif
  else
    print_usage ();
  endif
endfunction
