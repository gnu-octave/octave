a = [0, 2, 1; 2, 1, 2];

[q, r] = qr (a);

[qe, re] = qr (a, 0);

(size (q) == [2, 2] && size (r) == [2, 3]
 && abs (q * r - a) < sqrt (eps)
 && size (qe) == [2, 2] && size (re) == [2, 3]
 && abs (qe * re - a) < sqrt (eps))
