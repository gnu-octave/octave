rt2 = sqrt (2);
rt3 = sqrt (3);
x = [0, pi/6, pi/4, pi/3, 2*pi/3, 3*pi/4, 5*pi/6, pi];
v = [0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0];
all (abs (tan (x) - v) < sqrt (eps))
