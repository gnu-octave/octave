x = [3, 0, -3];
v = [8, 1, .125];
all (abs (pow2 (x) - v) < sqrt (eps))
