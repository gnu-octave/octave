prefer_zero_one_indexing = "true";
prefer_column_vectors = "true";
do_fortran_indexing = "true";
a = [9,8;7,6];
all (a([2,1],0:1) == [6;8])
