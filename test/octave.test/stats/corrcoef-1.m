x = rand (10);
cc1 = corrcoef (x);
cc2 = corrcoef (x, x);
(size (cc1) == [10, 10] && size (cc2) == [10, 10]
 && abs (cc1 - cc2) < sqrt (eps))
