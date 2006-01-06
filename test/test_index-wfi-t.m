%% Automatically generated from DejaGNU files

%% test/octave.test/index-wfi-t/s-1.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [];
%! assert(isempty (a));
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-2.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! assert(a(1),1);
%! warn_fortran_indexing = wfi;
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-3.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! assert(a(:),1);
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-4.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! assert(a(:,:),1);
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-5.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! assert(a(1,:),1);
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-6.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! assert(a(:,1),1);
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-7.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! assert(isempty (a(logical (0))));
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-8.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a(-1);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-9.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a(2);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-10.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a(2,:);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-11.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a(:,2);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-12.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a(-1,:);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-13.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a(:,-1);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-14.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a([1,2,3]);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-15.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a([1;2;3]);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-16.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a([1,2;3,4]);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-17.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a([0,1]);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-18.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a([0;1]);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-19.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a([-1,0]);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/s-20.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = 1;
%! fail("a([-1;0]);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-1.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(a(1),4);
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-2.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(a(2),3);
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-3.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(all (a(:) == a_prime));
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-4.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(all (a(1,:) == a));
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-5.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(a(:,3),2);
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-6.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(all (a(:,:) == a));
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-7.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(all (a(logical ([0,1,1,0])) == mid_a));
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-8.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! fail("a(0);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-9.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! fail("a(5);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-10.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! fail("a(0,1);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-11.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(isempty (a(logical (0),:)));
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-12.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! fail("a(:,0);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-13.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(isempty (a([])));
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-14.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(isempty (a([],:)));
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/v-15.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(isempty (a(:,[])));
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/m-1.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [1,2;3,4];
%! a_fvec = [1;3;2;4];
%! a_col_1 = [1;3];
%! a_col_2 = [2;4];
%! a_row_1 = [1,2];
%! a_row_2 = [3,4];
%! assert(all (all (a(:,:) == a)));
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/m-2.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [1,2;3,4];
%! a_fvec = [1;3;2;4];
%! a_col_1 = [1;3];
%! a_col_2 = [2;4];
%! a_row_1 = [1,2];
%! a_row_2 = [3,4];
%! assert(all (a(:) == a_fvec));
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/m-3.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [1,2;3,4];
%! a_fvec = [1;3;2;4];
%! a_col_1 = [1;3];
%! a_col_2 = [2;4];
%! a_row_1 = [1,2];
%! a_row_2 = [3,4];
%! fail("a(0);");
%! warn_fortran_indexing = wfi;

%% test/octave.test/index-wfi-t/m-4.m
%!test
%! wfi = warn_fortran_indexing;
%! warn_fortran_indexing = 1;
%! a = [1,2;3,4];
%! a_fvec = [1;3;2;4];
%! a_col_1 = [1;3];
%! a_col_2 = [2;4];
%! a_row_1 = [1,2];
%! a_row_2 = [3,4];
%! fail("a(2);","warning");
%! warn_fortran_indexing = wfi;

