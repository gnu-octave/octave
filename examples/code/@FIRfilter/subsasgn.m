function out = subsasgn (f, index, val)
  switch (index.type)
    case "."
      fld = index.subs;
      if (strcmp (fld, "polynomial"))
        out = f;
        out.polynomial = val;
      else
        error ("@FIRfilter/subsasgn: invalid property \"%s\"", fld);
      endif
    otherwise
      error ("@FIRfilter/subsasgn: Invalid index type")
  endswitch
endfunction
