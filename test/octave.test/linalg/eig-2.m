[v, d] = eig ([1, 2; 2, 1]);
x = 1 / sqrt (2);
((abs (d - [-1, 0; 0, 3]) < sqrt (eps))
 && (abs (v - [-x, x; x, x]) < sqrt (eps)))
