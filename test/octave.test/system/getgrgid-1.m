x = getgrent ();
y = getgrgid (x.gid);

strcmp (x.name, y.name) && x.gid == y.gid
