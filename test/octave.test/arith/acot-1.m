rt2 = sqrt (2);
rt3 = sqrt (3);
v = [pi/6, pi/4, pi/3, pi/2, -pi/3, -pi/4, -pi/6];
x = [rt3, 1, rt3/3, 0, -rt3/3, -1, -rt3];
all (abs (acot (x) - v) < sqrt (eps))


