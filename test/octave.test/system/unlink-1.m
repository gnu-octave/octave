nm = tmpnam ();
if ((id = fopen (nm, "wb")) > 0)
  [s, err] = stat (nm);
  if (! err && fclose (id) == 0 && unlink (nm) == 0)
    [s, err] = stat (nm);
    err < 0
  endif
endif
