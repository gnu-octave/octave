a = [1, 2, 3; 4, 5, 9; 7, 8, 6];
[u, s] = schur (a);
size (u) == [3, 3] && size (s) == [3, 3] && abs (s - u' * a * u) < sqrt (eps)
