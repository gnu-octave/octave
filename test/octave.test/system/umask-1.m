umask (0);
nm = tmpnam ();
id = fopen (nm, "w");
s1 = stat (nm);
fclose (id);
unlink (nm);

umask (777);
nm = tmpnam ();
id = fopen (nm, "w");
s2 = stat (nm);
fclose (id);
unlink (nm);

strcmp (s1.modestr, "-rw-rw-rw-") && strcmp (s2.modestr, "----------")
