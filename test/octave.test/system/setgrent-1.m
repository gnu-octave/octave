x = getgrent ();
setgrent ();
y = getgrent ();

strcmp (x.name, y.name) && x.gid == y.gid
