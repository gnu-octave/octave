(findstr ("ababab", "a") == [1, 3, 5]
 && findstr ("ababab", "aba") == [1, 3, 5]
 && findstr ("ababab", "aba", 0) == [1, 5])
