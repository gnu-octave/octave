v = [0, pi/2*i, pi*i, pi/2*i];
x = [1, 0, -1, 0];
all (abs (acosh (x) - v) < sqrt (eps))

