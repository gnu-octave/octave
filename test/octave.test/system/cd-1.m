xdir = pwd ();
cd /
d1 = pwd ();
cd (xdir)
strcmp ("/", d1) && strcmp (pwd (), xdir)
