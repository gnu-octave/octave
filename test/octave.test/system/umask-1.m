umask (0);
nm = tmpnam ();
id = fopen (nm, "wb");
s1 = stat (nm);
fclose (id);
unlink (nm);

umask (777);
nm = tmpnam ();
id = fopen (nm, "wb");
s2 = stat (nm);
fclose (id);
unlink (nm);

strcmp (s1.modestr, "-rw-rw-rw-") && strcmp (s2.modestr, "----------")
