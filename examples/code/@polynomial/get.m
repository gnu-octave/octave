function val = get (p, prop)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 1)
    val.poly = p.poly;
  else
    if (! ischar (prop))
      error ("@polynomial/get: PROPERTY must be a string");
    endif

    switch (prop)
      case "poly"
        val = p.poly;
      otherwise
        error ('@polynomial/get: invalid PROPERTY "%s"', prop);
    endswitch
  endif

endfunction
