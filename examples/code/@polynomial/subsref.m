function r = subsref (p, s)

  if (isempty (s))
    error ("@polynomial/subsref: missing index");
  endif

  switch (s(1).type)

    case "()"
      idx = s(1).subs;
      if (numel (idx) != 1)
        error ("@polynomial/subsref: need exactly one index");
      endif
      r = polyval (fliplr (p.poly), idx{1});

    case "{}"
      idx = s(1).subs;
      if (numel (idx) != 1)
        error ("@polynomial/subsref: need exactly one index");
      endif

      if (isnumeric (idx{1}))
        r = p.poly(idx{1}+1);
      else
        r = p.poly(idx{1});
      endif

    case "."
      fld = s.subs;
      if (! strcmp (fld, "poly"))
        error ('@polynomial/subsref: invalid property "%s"', fld);
      endif
      r = p.poly;

    otherwise
      error ("@polynomial/subsref: invalid subscript type");

  endswitch

  if (numel (s) > 1)
    r = subsref (r, s(2:end));
  endif

endfunction
