x = getgrent ();
y = getgrnam (x.name);

strcmp (x.name, y.name) && x.gid == y.gid
