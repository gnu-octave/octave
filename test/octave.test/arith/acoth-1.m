rt2 = sqrt (2);
rt3 = sqrt (3);
v = -i*[pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6];
x = i*[rt3, 1, rt3/3, -rt3/3, -1, -rt3];
all (abs (acoth (x) - v) < sqrt (eps))
