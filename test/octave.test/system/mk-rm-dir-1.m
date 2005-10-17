nm = tmpnam ();
e1 = mkdir (nm);
[s2, e2] = stat (nm);
e3 = rmdir (nm);
[s4, e4] = stat (nm);
(e1 && strcmp (s2.modestr(1), "d") && e3 && e4 < 0)
