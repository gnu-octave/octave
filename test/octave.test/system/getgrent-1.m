x = getgrent ();
(is_struct (x)
 && struct_contains (x, "name")
 && struct_contains (x, "passwd")
 && struct_contains (x, "gid")
 && struct_contains (x, "mem"))
