s = getpwent ();
(isstruct (s)
 && struct_contains (s, "name")
 && struct_contains (s, "passwd")
 && struct_contains (s, "uid")
 && struct_contains (s, "gid")
 && struct_contains (s, "gecos")
 && struct_contains (s, "dir")
 && struct_contains (s, "shell"))
