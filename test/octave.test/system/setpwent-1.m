x = getpwent ();
setpwent ();
y = getpwent ();

strcmp (x.name, y.name) && x.uid == y.uid && x.gid == y.gid
