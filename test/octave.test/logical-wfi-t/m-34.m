warn_fortran_indexing = 1;
a = [9,8;7,6];
all (all (a(logical ([1,1]),:) == [9,8;7,6]))
