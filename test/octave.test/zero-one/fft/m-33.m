prefer_zero_one_indexing = "false";
prefer_column_vectors = "false";
do_fortran_indexing = "true";
a = [9,8;7,6];
all (all (a([1,1],[2,1]) == [8,9;8,9]))
