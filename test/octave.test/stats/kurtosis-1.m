x = [-1; 0; 0; 0; 1];
y = [x, 2*x];
all (abs (kurtosis (y) - [-1.4, -1.4]) < sqrt (eps))
