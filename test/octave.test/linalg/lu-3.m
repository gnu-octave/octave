[l, u, p] = lu ([1, 2; 3, 4]);
(abs (l - [1, 0; 1/3, 1]) < sqrt (eps)
 && abs (u - [3, 4; 0, 2/3]) < sqrt (eps)
 && abs (p - [0, 1; 1, 0]) < sqrt (eps))
