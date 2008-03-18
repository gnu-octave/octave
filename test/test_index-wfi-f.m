## Copyright (C) 2006, 2007 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

%% Automatically generated from DejaGNU files

%% test/octave.test/index-wfi-f/s-1.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [];
%! assert(isempty (a));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-2.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! assert(a(1),1);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-3.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! assert(a(:),1);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-4.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! assert(a(:,:),1);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-5.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! assert(a(1,:),1);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-6.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! assert(a(:,1),1);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-7.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! assert(isempty (a(logical (0))));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-8.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a(-1)");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-9.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a(2);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-10.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a(2,:);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-11.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a(:,2);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-12.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a(-1,:);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-13.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a(:,-1);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-14.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([1,2,3]);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-15.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([1;2;3]);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-16.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([1,2;3,4]);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-17.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([0,1]);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-18.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([0;1]);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-19.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([-1,0]);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-20.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([-1;0]);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-1.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(a(1),4);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-2.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(a(2),3);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-3.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(all (a(:) == a_prime));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-4.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(all (a(1,:) == a));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-5.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(a(:,3),2);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-6.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(all (a(:,:) == a));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-7.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(all (a(logical ([0,1,1,0])) == mid_a));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-8.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! fail("a(0);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-9.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! fail("a(5);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-10.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! fail("a(0,1);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-11.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(isempty (a(logical (0),:)));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-12.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! fail("a(:,0);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-13.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(isempty (a([])));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-14.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(isempty (a([],:)));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-15.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(isempty (a(:,[])));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/m-1.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [1,2;3,4];
%! a_fvec = [1;3;2;4];
%! a_col_1 = [1;3];
%! a_col_2 = [2;4];
%! a_row_1 = [1,2];
%! a_row_2 = [3,4];
%! assert(all (all (a(:,:) == a)));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/m-2.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [1,2;3,4];
%! a_fvec = [1;3;2;4];
%! a_col_1 = [1;3];
%! a_col_2 = [2;4];
%! a_row_1 = [1,2];
%! a_row_2 = [3,4];
%! assert(all (a(:) == a_fvec));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/m-3.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [1,2;3,4];
%! a_fvec = [1;3;2;4];
%! a_col_1 = [1;3];
%! a_col_2 = [2;4];
%! a_row_1 = [1,2];
%! a_row_2 = [3,4];
%! fail("a(0);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/m-4.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [1,2;3,4];
%! a_fvec = [1;3;2;4];
%! a_col_1 = [1;3];
%! a_col_2 = [2;4];
%! a_row_1 = [1,2];
%! a_row_2 = [3,4];
%! assert(a(2),3);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/misc.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [1,2;3,4];
%! b = a;
%! b(:,:,2) = [5,6;7,8];
%! a1 = [1; 3; 2; 4];
%! a2 = [1, 3];
%! a3 = [1, 2; 3, 4];
%! a4 = [1; 3];
%! a5 = 1;
%! a6 = [1; 3];
%! a7 = [1, 2; 3, 4];
%! a8(:,:,1) = [1, 2; 3, 4];
%! a8(:,:,2) = [1, 2; 3, 4];
%! a9(:,:,1,1) = [1, 2; 3, 4];
%! a9(:,:,1,2) = [1, 2; 3, 4];
%! a10(:,:,1,1) = [1, 2; 3, 4];
%! a10(:,:,2,1) = [1, 2; 3, 4];
%! a10(:,:,1,2) = [1, 2; 3, 4];
%! a10(:,:,2,2) = [1, 2; 3, 4];
%! a11 = zeros (1, 0);
%! a12 = zeros (1, 0, 2);
%! a13 = zeros (1, 1, 0);
%! b1 = [1; 3; 2; 4; 5; 7; 6; 8];
%! b2 = [1, 2, 5, 6; 3, 4, 7, 8];
%! b3 = [1; 3];
%! b4(:,:,1) = [1, 2; 3, 4];
%! b4(:,:,2) = [5, 6; 7, 8];
%! b5 = [1; 3];
%! b6(:,:,1,1) = [1; 3];
%! b6(:,:,1,2) = [1; 3];
%! b7 = 5;
%! b8 = [5, 6];
%! b9 = [1, 2, 5, 6];
%! b10 = zeros (1, 0, 2);
%! b11 = zeros (1, 0);
%! b12 = [5; 7];
%! b13 = zeros (0, 1);
%! 
%! assert(a(:),a1);
%! assert(a(1:2), a2);
%! assert(a(:,:), a3);
%! assert(a(:,1), a4);
%! assert(a(1,1), a5);
%! assert(a(1:2,1), a6);
%! assert(a(:,:,1), a7);
%! assert(a(:,:,[1,1]), a8);
%! assert(a(:,:,1,[1,1]), a9);
%! assert(a(:,:,[1,1],[1,1]), a10);
%! assert(a(1,[]), a11);
%! assert(a(1,[],[1,1]), a12);
%! assert(a(1,1,[]), a13);
%! assert(b(:), b1);
%! assert(b(:,:), b2);
%! assert(b(:,1), b3);
%! assert(b(:,:,:), b4);
%! assert(b(:,1,1), b5);
%! assert(b(:,1,1,[1,1]), b6);
%! assert(b(1,3), b7);
%! assert(b(1,[3,4]), b8);
%! assert(b(1,1:4), b9);
%! assert(b(1,[],:), b10);
%! assert(b(1,[]), b11);
%! assert (b(:,3), b12);
%! assert (b([1,2],3), b12);
%! assert (b(true(2,1),3), b12);
%! assert (b(false(2,1),3), b13)
%! assert (b([],3), b13)
%! warning ("wfi.state", "Octave:fortran-indexing");
