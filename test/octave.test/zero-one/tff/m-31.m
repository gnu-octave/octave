prefer_zero_one_indexing = 1;
prefer_column_vectors = 0;
do_fortran_indexing = 0;
a = [9,8;7,6];
all (all (a([1,1],2:-1:1) == [8,9;6,7]))
