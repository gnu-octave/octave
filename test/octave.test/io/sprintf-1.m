[s, msg, status] = sprintf ("%s: %d\n", "test", 1);

s == "test: 1\n" && isstr (msg) && status == 2
