rt2 = sqrt (2);
rt3 = sqrt (3);
v = [0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0];
y = [0, rt3, 1, rt3, -rt3, -1, -rt3, 0];
x = [1, 3, 1, 1, 1, 1, 3, 1];
all (abs (atan2 (y, x) - v) < sqrt (eps))

