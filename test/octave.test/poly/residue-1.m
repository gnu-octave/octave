b = [1, 1, 1];
a = [1, -5, 8, -4];
[r, p, k, e] = residue (b, a);
(abs (r - [-2; 7; 3]) < sqrt (eps)
 && abs (p - [2; 2; 1]) < sqrt (eps)
 && isempty (k)
 && e == [1; 2; 1])
