t1 = exp (0.5 * log (i));
t2 = exp (0.5 * log (1-i));
all (all (abs (sqrt ([4, -4; i, 1-i]) - [2, 2i; t1, t2]) < sqrt (eps)))
