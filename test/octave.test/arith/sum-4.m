(all (sum ([1, 2; 3, 4], 1) == [4, 6])
 && all (sum ([1, 2; 3, 4], 2) == [3; 7])
 && sum (zeros (1, 0)) == 0
 && all (size (sum (zeros (1, 0), 1)) == [1, 0])
 && sum (zeros (1, 0), 2) == 0
 && sum (zeros (0, 1)) == 0
 && sum (zeros (0, 1), 1) == 0
 && all (size (sum (zeros (0, 1), 2)) == [0, 1])
 && all (size (sum (zeros (2, 0))) == [1, 0])
 && all (size (sum (zeros (2, 0), 1)) == [1, 0])
 && all (sum (zeros (2, 0), 2) == [0; 0])
 && all (sum (zeros (0, 2)) == [0, 0])
 && all (sum (zeros (0, 2), 1) == [0, 0])
 && all (size (sum (zeros (0, 2), 2)) == [0, 1]))
