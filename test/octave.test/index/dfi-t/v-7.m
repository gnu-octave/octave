do_fortran_indexing = 1;
a = [4,3,2,1];
a_prime = [4;3;2;1];
mid_a = [3,2];
all (a([0,1,1,0]) == mid_a)
