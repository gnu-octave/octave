a = [1, 2, 3; 5, 4, 6; 8, 7, 9];
[p, h] = hess (a);
size (p) == [3, 3] && size (h) == [3, 3] && abs (a - p * h * p') < sqrt (eps)
