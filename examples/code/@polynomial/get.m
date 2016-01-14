function s = get (p, f)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 1)
    s.poly = p.poly;
  else
    if (! ischar (f))
      error ("@polynomial/get: expecting the property to be a string");
    endif

    switch (f)
      case "poly"
        s = p.poly;
      otherwise
        error ("@polynomial/get: invalid property %s", f);
    endswitch
  endif

endfunction
