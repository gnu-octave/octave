prefer_zero_one_indexing = 0;
prefer_column_vectors = 0;
do_fortran_indexing = 1;
a = [9,8,7,6];
all (a([0,1,1,0]) == [8,7])
