prefer_column_vectors = 0;
do_fortran_indexing = 0;
a = [9,8;7,6];
all (all (a(logical ([1,1]),logical ([1,1])) == [9,8;7,6]))
