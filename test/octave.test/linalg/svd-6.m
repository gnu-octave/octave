a = [1, 2; 3, 4; 5, 6];
[u, s, v] = svd (a, 1);
(size (u) == [3, 2] && size (s) == [2, 2] && size (v) == [2, 2]
 && abs (a - u * s * v') < sqrt (eps))
