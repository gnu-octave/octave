x = getpwuid (getuid ());
(strcmp (x.dir, tilde_expand ("~"))
 && strcmp (x.dir, tilde_expand (sprintf ("~%s", x.name)))
 && strcmp ("foobar", tilde_expand ("foobar")))
