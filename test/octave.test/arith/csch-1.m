x = [pi/2*i, 3*pi/2*i];
v = [-i, i];
all (abs (csch (x) - v) < sqrt (eps))
