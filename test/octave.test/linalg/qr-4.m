a = [0, 2; 2, 1; 1, 2];
[q, r, p] = qr (a, 0);
(size (q) == [3, 2] && size (r) == [2, 2] && size (p) == [1, 2]
 && abs (a (:, p) - q * r) < sqrt (eps))
