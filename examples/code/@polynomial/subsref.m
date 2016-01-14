function r = subsref (a, s)

  if (isempty (s))
    error ("@polynomial/subsref: missing index");
  endif

  switch (s(1).type)

    case "()"
      ind = s(1).subs;
      if (numel (ind) != 1)
        error ("@polynomial/subsref: need exactly one index");
      endif
      r = polyval (fliplr (a.poly), ind{1});

    case "{}"
      ind = s(1).subs;
      if (numel (ind) != 1)
        error ("@polynomial/subsref: need exactly one index");
      endif

      if (isnumeric (ind{1}))
        r = a.poly(ind{1}+1);
      else
        r = a.poly(ind{1});
      endif

    case "."
      fld = s.subs;
      if (! strcmp (fld, "poly"))
        error ("@polynomial/subsref: invalid property '%s'", fld);
      endif
      r = a.poly;

    otherwise
      error ("@polynomial/subsref: invalid subscript type");

  endswitch

  if (numel (s) > 1)
    r = subsref (r, s(2:end));
  endif

endfunction
