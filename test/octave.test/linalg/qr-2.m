a = [0, 2, 1; 2, 1, 2];

[q, r, p] = qr (a);  # not giving right dimensions. FIXME

[qe, re, pe] = qr (a, 0);

(size (q) == [2, 2] && size (r) == [2, 3] && size (p) == [3, 3]
 && abs (q * r - a * p) < sqrt (eps)
 && size (qe) == [2, 2] && size (re) == [2, 3] && size (pe) == [1, 3]
 && abs (qe * re - a(:,pe)) < sqrt (eps))
