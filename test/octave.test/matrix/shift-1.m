a = [1, 2, 3];
b = [4, 5, 6];
c = [7, 8, 9];

r = [a, b, c];
m = [a; b; c];

(shift (r, 3) == [c, a, b]
 && shift (r, -6) == [c, a, b]
 && shift (r, -3) == [b, c, a]
 && shift (m, 1) == [c; a; b]
 && shift (m, -2) == [c; a; b])
