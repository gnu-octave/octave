x = [0, pi/2*i, pi*i, 3*pi/2*i];
v = [1, 0, -1, 0];
all (abs (cosh (x) - v) < sqrt (eps))
