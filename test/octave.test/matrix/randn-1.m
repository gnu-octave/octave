randn ("seed", 0.5);
r1 = randn (100);
randn ("seed", 0.5);
r2 = randn (100);
all (all (r1 == r2))
