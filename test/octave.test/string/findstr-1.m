(findstr ("abababa", "a") == [1, 3, 5, 7]
 && findstr ("abababa", "aba") == [1, 3, 5]
 && findstr ("abababa", "aba", 0) == [1, 5])
