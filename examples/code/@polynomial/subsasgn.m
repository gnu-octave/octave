function p = subsasgn (p, s, val)

  if (isempty (s))
    error ("@polynomial/subsasgn: needs index");
  endif

  switch (s(1).type)

    case "{}"
      ind = s(1).subs;
      if (numel (ind) != 1)
        error ("@polynomial/subsasgn: need exactly one index");
      elseif (numel (s) != 1)
        error ("@polynomial/subsasgn: chained subscripts not allowed for {}");
      endif

      if (isnumeric (ind{1}))
        p.poly(ind{1}+1) = val;
      else
        p.poly(ind{1}) = val;
      endif

    case "."
      fld = s(1).subs;
      if (! strcmp (fld, "poly"))
        error ('@polynomial/subsasgn: invalid property "%s"', fld);
      endif
      if (numel (s) == 1)
        p.poly = val;
      else
        p.poly = subsasgn (p.poly, s(2:end), val);
      endif

    otherwise
      error ("@polynomial/subsasgn: invalid subscript type");

  endswitch

endfunction
