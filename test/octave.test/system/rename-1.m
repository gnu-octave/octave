from = tmpnam ();
to = tmpnam ();
id = fopen (from, "wb");
if (id > 0 && fclose (id) == 0)
  [s, e] = stat (from);
  if (! e)
    if (rename (from, to) == 0)
      [s, e] = stat (from);
      if (e < 0)
      	[s, e] = stat (to);
      	e == 0
      	unlink (to);
      endif
    endif
  endif
endif
