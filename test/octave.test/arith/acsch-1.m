v = [pi/2*i, -pi/2*i];
x = [-i, i];
all (abs (acsch (x) - v) < sqrt (eps))

