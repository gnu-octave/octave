t1 = clock;
t2 = str2num (strftime ("[%Y, %m, %e, %k, %M, %S]", localtime (time ())));
etime (t1, t2) < 1
