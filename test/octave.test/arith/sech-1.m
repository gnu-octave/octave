x = [0, pi*i];
v = [1, -1];
all (abs (sech (x) - v) < sqrt (eps))
