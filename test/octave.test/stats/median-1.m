x = [1, 2, 3, 4, 5, 6];
x2 = x';
y = [1, 2, 3, 4, 5, 6, 7];
y2 = y';

(median (x) == median (x2) && median (x) == 3.5
 && median (y) == median (y2) && median (y) == 4
 && median ([x2, 2*x2]) == [3.5, 7]
 && median ([y2, 3*y2]) == [4, 12])
