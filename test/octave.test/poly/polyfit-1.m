x = [-2, -1, 0, 1, 2];
all (all (abs (polyfit (x, x.^2+x+1, 2) - [1, 1, 1]) < sqrt (eps)))
