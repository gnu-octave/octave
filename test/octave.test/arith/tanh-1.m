x = [0, pi*i];
v = [0, 0];
all (abs (tanh (x) - v) < sqrt (eps))
