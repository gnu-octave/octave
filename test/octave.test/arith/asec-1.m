rt2 = sqrt (2);
rt3 = sqrt (3);
v = [0, pi/6, pi/4, pi/3, 2*pi/3, 3*pi/4, 5*pi/6, pi];
x = [1, 2*rt3/3, rt2, 2, -2, -rt2, -2*rt3/3, -1];
all (abs (asec (x) - v) < sqrt (eps))


