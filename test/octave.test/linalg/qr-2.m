a = [0, 2, 1; 2, 1, 2];
[q, r, p] = qr (a);
(size (q) == [2, 2] && size (r) == [2, 3] && size (p) == [3, 3]
 && abs (a * p - q * r) < sqrt (eps))
