prefer_zero_one_indexing = 1;
prefer_column_vectors = 0;
do_fortran_indexing = 1;
a = [9,8;7,6];
all (a([1,1,1,1]) == [9;7;8;6])
