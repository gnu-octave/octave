rt2 = sqrt (2);
rt3 = sqrt (3);
x = [pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6];
v = [2, rt2, 2*rt3/3, 1, 2*rt3/3, rt2, 2];
all (abs (csc (x) - v) < sqrt (eps))
