rt2 = sqrt (2);
rt3 = sqrt (3);
v = [pi/6, pi/4, pi/3, pi/2, pi/3, pi/4, pi/6];
x = [2, rt2, 2*rt3/3, 1, 2*rt3/3, rt2, 2];
all (abs (acsc (x) - v) < sqrt (eps))

