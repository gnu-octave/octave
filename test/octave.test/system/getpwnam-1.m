x = getpwent ();
y = getpwnam (x.name);

strcmp (x.name, y.name) && x.uid == y.uid && x.gid == y.gid
