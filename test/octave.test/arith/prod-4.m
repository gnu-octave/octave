(all (prod ([1, 2; 3, 4], 1) == [3, 8])
 && all (prod ([1, 2; 3, 4], 2) == [2; 12])
 && prod (zeros (1, 0)) == 1
 && all (size (prod (zeros (1, 0), 1)) == [1, 0])
 && prod (zeros (1, 0), 2) == 1
 && prod (zeros (0, 1)) == 1
 && prod (zeros (0, 1), 1) == 1
 && all (size (prod (zeros (0, 1), 2)) == [0, 1])
 && all (size (prod (zeros (2, 0))) == [1, 0])
 && all (size (prod (zeros (2, 0), 1)) == [1, 0])
 && all (prod (zeros (2, 0), 2) == [1; 1])
 && all (prod (zeros (0, 2)) == [1, 1])
 && all (prod (zeros (0, 2), 1) == [1, 1])
 && all (size (prod (zeros (0, 2), 2)) == [0, 1]))
