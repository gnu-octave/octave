a = [1, 2, 3; 4, 5, 6];
[u, s, v] = svd (a);
(size (u) == [2, 2] && size (s) == [2, 3] && size (v) == [3, 3]
 && abs (a - u * s * v') < sqrt (eps))
