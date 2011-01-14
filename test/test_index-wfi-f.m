## Copyright (C) 2006-2011 John W. Eaton
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

%% test/octave.test/index-wfi-f/s-1.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [];
%! assert(isempty (a));
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-2.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! assert(a(1),1);
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-3.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! assert(a(:),1);
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-4.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! assert(a(:,:),1);
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-5.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! assert(a(1,:),1);
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-6.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! assert(a(:,1),1);
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-7.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! assert(isempty (a(logical (0))));
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-8.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a(-1)");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-9.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a(2);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-10.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a(2,:);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-11.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a(:,2);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-12.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a(-1,:);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-13.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a(:,-1);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-14.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([1,2,3]);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-15.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([1;2;3]);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-16.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([1,2;3,4]);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-17.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([0,1]);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-18.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([0;1]);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-19.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([-1,0]);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/s-20.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 1;
%! fail("a([-1;0]);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-1.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(a(1),4);
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-2.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(a(2),3);
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-3.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(all (a(:) == a_prime));
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-4.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(all (a(1,:) == a));
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-5.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(a(:,3),2);
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-6.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(all (a(:,:) == a));
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-7.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(all (a(logical ([0,1,1,0])) == mid_a));
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-8.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! fail("a(0);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-9.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! fail("a(5);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-10.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! fail("a(0,1);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-11.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(isempty (a(logical (0),:)));
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-12.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! fail("a(:,0);");
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-13.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(isempty (a([])));
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-14.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(isempty (a([],:)));
%! warning (wfi.state, "Octave:fortran-indexing");

%% test/octave.test/index-wfi-f/v-15.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];
%! assert(isempty (a(:,[])));
%! warning (wfi.state, "Octave:fortran-indexing");

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
%! warning (wfi.state, "Octave:fortran-indexing");

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
%! warning (wfi.state, "Octave:fortran-indexing");

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
%! warning (wfi.state, "Octave:fortran-indexing");

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
%! warning (wfi.state, "Octave:fortran-indexing");

%% Additional tests
%!shared a, b
%! a = [1,2;3,4];
%! b = a;
%! b(:,:,2) = [5,6;7,8];

%!assert (a(:), [1;3;2;4]);
%!assert (a(1:2), [1,3]);
%!assert (a(:,:), [1,2;3,4]);
%!assert (a(:,1), [1;3]);
%!assert (a(1,1), 1);
%!assert (a(1:2,1), [1;3]);
%!assert (a(:,:,1), [1,2;3,4]);

%!test
%! c(:,:,1) = [1,2;3,4];
%! c(:,:,2) = [1,2;3,4];
%! assert (a(:,:,[1,1]),c)

%!test
%! c(:,:,1,1) = [1,2;3,4];
%! c(:,:,1,2) = [1,2;3,4];
%! assert (a(:,:,1,[1,1]),c)

%!test
%! c(:,:,1,1) = [1,2;3,4];
%! c(:,:,2,1) = [1,2;3,4];
%! c(:,:,1,2) = [1,2;3,4];
%! c(:,:,2,2) = [1,2;3,4];
%! assert (a(:,:,[1,1],[1,1]),c)

%!assert (a(1,[]), zeros(1,0));
%!assert (a(1,[],[1,1]), zeros(1,0,2));
%!assert (a(1,1,[]), zeros(1,1,0));

%!test
%! c (1:10,1) = 1:10;
%! assert (c, [1:10]');

%!assert (b(:), [1; 3; 2; 4; 5; 7; 6; 8]);
%!assert (b(:,:), [1, 2, 5, 6; 3, 4, 7, 8]);
%!assert (b(:,1), [1;3]);
%!assert (b(:,:,:), reshape ([1,3,2,4,5,7,6,8],[2,2,2]));
%!assert (b(:,1,1), [1;3]);
%!assert (b(:,1,1,[1,1]),reshape([1,3,1,3],[2,1,1,2]));
%!assert (b(1,3), 5);
%!assert (b(1,[3,4]), [5,6]);
%!assert (b(1,1:4), [1,2,5,6]);
%!assert (b(1,[],:), zeros (1,0,2));
%!assert (b(1,[]), zeros(1,0));
%!assert (b(:,3), [5;7])
%!assert (b([1,2],3), [5;7])
%!assert (b(true(2,1),3), [5;7])
%!assert (b(false(2,1),3), zeros(0,1))
%!assert (b([],3), zeros(0,1));

%!shared x
%! # Dummy shared block to clear any previous definitions
%! x = 1;

%!test
%! a(1,:) = [1,3];
%! assert (a, [1,3]);

%!test
%! a(1,:) = [1;3];
%! assert (a, [1,3]);

%!test
%! a(:,1) = [1;3];
%! assert (a, [1;3]);

%!test
%! a = [1,2;3,4];
%! b (1,:,:) = a;
%! assert (b, reshape (a, [1,2,2]));

%!test
%! a(1,1:4,2) = reshape (1:4, [1,1,4]);
%! b(:,:,2) = 1:4;
%! assert (a, b);

%!test
%! a(:,:,:) = 1:4; 
%! assert (a, [1:4]);

%!test
%! a(:,:,1) = 1:4;;
%! assert (a, [1:4]);

%!test
%! a(:,:,1) = [1:4]';
%! assert (a, [1:4]');

%!test
%! a(:,:,1) = reshape(1:4,[1,1,4]);
%! assert (a, [1:4]');

%!test
%! a(:,1,:) = 1:4;
%! assert (a, reshape (1:4,[1,1,4]));

%!test
%! a(:,1,:) = [1:4]';
%! assert (a, [1:4]');

%!test
%! a(:,1,:) = reshape(1:4,[1,1,4]);;
%! assert (a, [1:4]');

%!test
%! a(1,:,:) = 1:4;
%! assert (a, reshape (1:4,[1,1,4]));

%!test
%! a(1,:,:) = [1:4]';
%! assert (a, [1:4]);

%!test
%! a(1,:,:) = reshape(1:4,[1,1,4]);
%! assert (a, [1:4]);

%!test
%! a(1,:,:,:) = reshape(1:4,[1,1,4]);
%! assert (a, reshape (1:4,[1,1,1,4]));

%!error (a(1:2,1:2) = 1:4)
