[t1, u1, s1] = cputime ();
for i = 1:200
  sin (i);
endfor
[t2, u2, s2] = cputime ();
t1 == u1 + s1 && t2 == u2 + s2 && t2 >= t1 && u2 >= u2 && s2 >= s2
