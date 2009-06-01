function p = subsasgn (p, index, val)
  index.type
  index.subs
  switch (index.type)
    case "()"
      ind = index.subs;
      if ( (any (ind{:}>length(p.poly)))
        || (any (ind{:}<0)) )
        error ("subsasgn: subscript out of range");
      endif
      p.poly(ind{:}) = val;
    case "."
      fld = index.subs;
      if (strcmp (fld, "poly"))
        p.poly = val;
      else
        error ("@polynomial/subsref: invalid property \"%s\"", fld);
      endif
  endswitch
endfunction
