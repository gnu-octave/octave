x = [0, pi/2*i, pi*i, 3*pi/2*i];
v = [0, i, 0, -i];
all (abs (sinh (x) - v) < sqrt (eps))
