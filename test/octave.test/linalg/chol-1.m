rt2 = sqrt (2);
all (all (abs (chol ([2, 1; 1, 1]) - [rt2, 1/rt2; 0, 1/rt2]) < sqrt (eps)))
