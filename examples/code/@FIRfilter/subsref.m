function r = subsref (f, x)

  switch (x.type)

    case "()"
      n = f.polynomial;
      r = filter (n.poly, 1, x.subs{1});

    case "."
      fld = x.subs;
      if (! strcmp (fld, "polynomial"))
        error ('@FIRfilter/subsref: invalid property "%s"', fld);
      endif
      r = f.polynomial;

    otherwise
      error ("@FIRfilter/subsref: invalid subscript type for FIR filter");

  endswitch

endfunction
