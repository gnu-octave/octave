x = rand (10);
cx1 = cov (x);
cx2 = cov (x, x);
size (cx1) == [10, 10] && size (cx2) == [10, 10] && cx1 == cx2
