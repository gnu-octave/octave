[files, status, msg] = readdir ("/");
iscell (files) && status == 0 && msg == ""
