prefer_zero_one_indexing = 1;
prefer_column_vectors = 1;
do_fortran_indexing = 1;
a = [9,8;7,6];
all (all (a(2:-1:1,[1,1]) == [7,6;9,8]))
