x = getpwent ();
y = getpwuid (x.uid);

strcmp (x.name, y.name) && x.uid == y.uid && x.gid == y.gid
