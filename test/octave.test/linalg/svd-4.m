a = [1, 2; 3, 4; 5, 6];
[u, s, v] = svd (a);
(size (u) == [3, 3] && size (s) == [3, 2] && size (v) == [2, 2]
 && abs (a - u * s * v') < sqrt (eps))
