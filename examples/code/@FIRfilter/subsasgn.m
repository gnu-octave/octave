function out = subsasgn (f, index, val)

  switch (index.type)
    case "."
      fld = index.subs;
      if (! strcmp (fld, "polynomial"))
        error ('@FIRfilter/subsasgn: invalid property "%s"', fld);
      endif
      out = f;
      out.polynomial = val;

    otherwise
      error ("@FIRfilter/subsasgn: Invalid index type")
  endswitch

endfunction
