[s, err, msg] = lstat ("/");
(err == 0
 && isstruct (s)
 && struct_contains (s, "dev")
 && struct_contains (s, "ino")
 && struct_contains (s, "modestr")
 && struct_contains (s, "nlink")
 && struct_contains (s, "uid")
 && struct_contains (s, "gid")
 && struct_contains (s, "size")
 && struct_contains (s, "atime")
 && struct_contains (s, "mtime")
 && struct_contains (s, "ctime")
 && isstr (msg))
