prefer_column_vectors = 1;
do_fortran_indexing = 1;
a = [9,8;7,6];
a(1,logical ([0,1])) == 8
