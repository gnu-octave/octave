[v, d] = eig ([1, 2; 2, 1]);
x = 1 / sqrt (2);
(abs (d - [3, 0; 0, -1] < sqrt (eps))
 && abs (v - [x, -x; x, x] < sqrt (eps)))
