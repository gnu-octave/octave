a = [0, 2; 2, 1; 1, 2];

[q, r, p] = qr (a);

[qe, re, pe] = qr (a, 0);

(size (q) == [3, 3] && size (r) == [3, 2] && size (p) == [2, 2]
 && abs (a * p - q * r) < sqrt (eps)
 && size (qe) == [3, 2] && size (re) == [2, 2] && size (pe) == [1, 2]
 && abs (a(:,pe) - qe * re) < sqrt (eps))
