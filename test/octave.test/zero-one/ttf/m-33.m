prefer_zero_one_indexing = "true";
prefer_column_vectors = "true";
do_fortran_indexing = "false";
a = [9,8;7,6];
all (all (a([1,1],[2,1]) == [8,9;6,7]))
