[u, s, v] = svd ([1, 2; 2, 1]);
x = 1 / sqrt (2);
((abs (u - [-x, -x; -x, x]) < sqrt (eps))
 && (abs (s - [3, 0; 0, 1]) < sqrt (eps))
 && (abs (v - [-x, x; -x, -x]) < sqrt (eps)))
