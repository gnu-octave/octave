x = [pi/2*i, 3*pi/2*i];
v = [0, 0];
all (abs (coth (x) - v) < sqrt (eps))
