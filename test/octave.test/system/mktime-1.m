t = time ();
fix (mktime (localtime (t))) == fix (t)
