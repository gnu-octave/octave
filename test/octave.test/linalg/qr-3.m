a = [0, 2; 2, 1; 1, 2];

[q, r] = qr (a);

[qe, re] = qr (a, 0);

(size (q) == [3, 3] && size (r) == [3, 2]
 && abs (a - q * r) < sqrt (eps)
 && size (qe) == [3, 2] && size (re) == [2, 2]
 && abs (a - qe * re) < sqrt (eps))
