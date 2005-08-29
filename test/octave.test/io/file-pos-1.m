nm = tmpnam ();
id = fopen (nm, "wb");
if (id > 0)
  fprintf (id, "%d\n", 1:100);
  fclose (id);
  id = fopen (nm, "rb");
  if (id > 0)
    for i = 1:101
      fgets (id);
    endfor
    if (feof (id))
      fclose (id);
      id = fopen (nm, "rb");
      pos_one = ftell (id);
      s_one = fgets (id);
      for i = 1:48
	s = fgets (id);
      endfor
      pos_fifty = ftell (id);
      s_fifty = fgets (id);
      fseek (id, pos_one, SEEK_SET);
      s_one_x = fgets (id);
      fseek (id, pos_fifty, SEEK_SET);
      s_fifty_x = fgets (id);
      if (s_one == s_one_x && s_fifty == s_fifty_x)
	frewind (id);
	s_one_x = fgets (id);
	if (s_one == s_one_x)
	  printf ("ok\n");
	endif
      endif
    endif
  endif
endif
unlink (nm);	  
