x = [-2, -1, 0, 1, 2];
all (all (polyfit (x, x.^2+x+1, 2) - [1; 1; 1] < 4*eps))
