warn_fortran_indexing = 0;
a = [9,8;7,6];
all (all (a(logical ([1,1]),2:-1:1) == [8,9;6,7]))
