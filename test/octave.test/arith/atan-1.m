rt2 = sqrt (2);
rt3 = sqrt (3);
v = [0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0];
x = [0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0];
all (abs (atan (x) - v) < sqrt (eps))


