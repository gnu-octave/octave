x = ones (10, 2);
y = [1, 3];
std (x) == [0, 0] && abs (std (y) - sqrt (2)) < sqrt (eps)
