function b = subsref (a, s)
  switch s.type
    case "()"
      ind = s.subs;
      b = polyval (fliplr(a.poly), ind{:});
    case "{}"
      ind = s.subs;
      b = polynomial (a.poly(ind{:}));
    case "."
      fld = s.subs;
      if (strcmp (fld, "poly"))
	b = a.poly;
      else
	error ("subsref: unrecognized property \"%s\" of a polynomial", fld);
      endif
  endswitch
endfunction
