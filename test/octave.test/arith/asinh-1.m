v = [0, pi/2*i, 0, -pi/2*i];
x = [0, i, 0, -i];
all (abs (asinh (x) - v) < sqrt (eps))

