(size (reshape (rand (4, 4), 2, 8)) == [2, 8]
 && size (reshape (rand (4, 4), 8, 2)) == [8, 2]
 && size (reshape (rand (15, 4), 1, 60)) == [1, 60]
 && size (reshape (rand (15, 4), 60, 1)) == [60, 1])
