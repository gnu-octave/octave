a = [0, 2; 2, 1; 1, 2];
[q, r, p] = qr (a);
(size (q) == [3, 3] && size (r) == [3, 2] && size (p) == [2, 2]
 && abs (a - q * r * p) < sqrt (eps))
