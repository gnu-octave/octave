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

%% test/octave.test/matrix/all-1.m
%!test
%! x = ones (3);
%! x(1,1) = 0;
%! assert((all (all (rand (3) + 1) == [1, 1, 1]) == 1
%! && all (all (x) == [0, 1, 1]) == 1
%! && all (x, 1) == [0, 1, 1]
%! && all (x, 2) == [0; 1; 1]));

%% test/octave.test/matrix/all-2.m
%!error <Invalid call to all.*> all ();

%% test/octave.test/matrix/all-3.m
%!error <Invalid call to all.*> all (1, 2, 3);

%% test/octave.test/matrix/any-1.m
%!test
%! x = zeros (3);
%! x(3,3) = 1;
%! assert((all (any (x) == [0, 0, 1]) == 1
%! && all (any (ones (3)) == [1, 1, 1]) == 1
%! && any (x, 1) == [0, 0, 1]
%! && any (x, 2) == [0; 0; 1]));

%% test/octave.test/matrix/any-2.m
%!error <Invalid call to any.*> any ();

%% test/octave.test/matrix/any-3.m
%!error <Invalid call to any.*> any (1, 2, 3);

%% test/octave.test/matrix/find-1.m
%!assert((find ([1, 0, 1, 0, 1]) == [1, 3, 5]
%! && find ([1; 0; 3; 0; 1]) == [1; 3; 5]
%! && find ([0, 0, 2; 0, 3, 0; -1, 0, 0]) == [3; 5; 7]));

%% test/octave.test/matrix/find-2.m
%!test
%! [i, j, v] = find ([0, 0, 2; 0, 3, 0; -1, 0, 0]);
%! 
%! assert(i == [3; 2; 1] && j == [1; 2; 3] && v == [-1; 3; 2]);

%% test/octave.test/matrix/find-3.m
%!error <Invalid call to find.*> find ();

%% test/octave.test/matrix/reshape-1.m
%!assert((size (reshape (rand (4, 4), 2, 8)) == [2, 8]
%! && size (reshape (rand (4, 4), 8, 2)) == [8, 2]
%! && size (reshape (rand (15, 4), 1, 60)) == [1, 60]
%! && size (reshape (rand (15, 4), 60, 1)) == [60, 1]));

%% test/octave.test/matrix/reshape-2.m
%!test
%! s.a = 1;
%! fail("reshape (s, 2, 3)");

%% test/octave.test/matrix/reshape-3.m
%!error <Invalid call to reshape.*> reshape ();

%% test/octave.test/matrix/reshape-4.m
%!error reshape (1, 2, 3, 4);

%% test/octave.test/matrix/sort-1.m
%!test
%! a = [1, 2; 2, 3; 3, 1];
%! s = [1, 1; 2, 2; 3, 3];
%! i = [1, 3; 2, 1; 3, 2];
%! [xs, xi] = sort (a);
%! assert(sort (a) == s && xs == s && xi == i);

%% test/octave.test/matrix/sort-2.m
%!error <Invalid call to sort.*> sort ();

%% test/octave.test/matrix/sort-3.m
%!error <Invalid call to sort.*> sort (1, 2, 3, 4);



%% test/octave.test/matrix/eye-1.m
%!test
%! i33 = [1, 0, 0; 0, 1, 0; 0, 0, 1];
%! i23 = [1, 0, 0; 0, 1, 0];
%! assert((eye (3) == i33 && eye (size (i33)) == i33 && eye (3, 3) == i33
%! && eye (2, 3) == i23 && eye (3, 2) == i23'));

%% test/octave.test/matrix/eye-2.m
%!error <Invalid call to eye.*> eye (1, 2, 3);

%% test/octave.test/matrix/ones-1.m
%!test
%! x33 = [1, 1, 1; 1, 1, 1; 1, 1, 1];
%! x23 = [1, 1, 1; 1, 1, 1];
%! assert((ones (3) == x33 && ones (size (x33)) == x33 && ones (3, 3) == x33
%! && ones (2, 3) == x23 && ones (3, 2) == x23'));

%% test/octave.test/matrix/ones-2.m
%!assert(all (size (ones (3, 4, 5)) == [3, 4, 5]));

%% test/octave.test/matrix/zeros-1.m
%!test
%! x33 = [0, 0, 0; 0, 0, 0; 0, 0, 0];
%! x23 = [0, 0, 0; 0, 0, 0];
%! assert((zeros (3) == x33 && zeros (size (x33)) == x33 && zeros (3, 3) == x33
%! && zeros (2, 3) == x23 && zeros (3, 2) == x23'));

%% test/octave.test/matrix/zeros-2.m
%!assert(all (size (zeros (3, 4, 5)) == [3, 4, 5]));

%% test/octave.test/matrix/rand-1.m
%!test
%! rand ("seed", 0.5);
%! r1 = rand (100);
%! rand ("seed", 0.5);
%! r2 = rand (100);
%! assert(rand (100) < 1 && rand (100) > 0 && r1 == r2);

%% test/octave.test/matrix/rand-2.m
%!assert(all (size (rand (1, 2, 3)) == [1, 2, 3]));

%% test/octave.test/matrix/randn-1.m
%!test
%! randn ("seed", 0.5);
%! r1 = randn (100);
%! randn ("seed", 0.5);
%! r2 = randn (100);
%! assert(all (all (r1 == r2)));

%% test/octave.test/matrix/randn-2.m
%!assert(all (size (randn (1, 2, 3)) == [1, 2, 3]));

%% test/octave.test/matrix/diag-1.m
%!test
%! d = [1; 2; 3];
%! 
%! d0 = [1, 0, 0;
%! 0, 2, 0;
%! 0, 0, 3];
%! 
%! d1 = [0, 1, 0, 0;
%! 0, 0, 2, 0;
%! 0, 0, 0, 3;
%! 0, 0, 0, 0];
%! 
%! d2 = [0, 0, 1, 0, 0;
%! 0, 0, 0, 2, 0;
%! 0, 0, 0, 0, 3;
%! 0, 0, 0, 0, 0;
%! 0, 0, 0, 0, 0];
%! 
%! dm1 = [0, 0, 0, 0;
%! 1, 0, 0, 0;
%! 0, 2, 0, 0;
%! 0, 0, 3, 0];
%! 
%! dm2 = [0, 0, 0, 0, 0;
%! 0, 0, 0, 0, 0;
%! 1, 0, 0, 0, 0;
%! 0, 2, 0, 0, 0;
%! 0, 0, 3, 0, 0];
%! 
%! assert((diag (d) == d0 && diag (d, 1) == d1 && diag (d, 2) == d2
%! && diag (d, -1) == dm1 && diag (d, -2) == dm2
%! && diag (d0) == d && diag (d1, 1) == d && diag (dm1, -1) == d));

%% test/octave.test/matrix/diag-2.m
%!error <Invalid call to diag.*> diag ();

%% test/octave.test/matrix/diag-3.m
%!error <Invalid call to diag.*> diag (1, 2, 3);

%% test/octave.test/matrix/linspace-1.m
%!test
%! x1 = linspace (1, 2);
%! x2 = linspace (1, 2, 10);
%! x3 = linspace (1, -2, 10);
%! assert((size (x1) == [1, 100] && x1(1) == 1 && x1(100) == 2
%! && size (x2) == [1, 10] && x2(1) == 1 && x2(10) == 2
%! && size (x3) == [1, 10] && x3(1) == 1 && x3(10) == -2));

%% test/octave.test/matrix/linspace-2.m
%!test
%! warn_fortran_indexing = 0;
%! assert(all (linspace ([1, 2; 3, 4], 5, 6) == linspace (1, 5, 6)));

%% test/octave.test/matrix/linspace-3.m
%!error <Invalid call to linspace.*> linspace ();

%% test/octave.test/matrix/linspace-4.m
%!error <Invalid call to linspace.*> linspace (1, 2, 3, 4);

%% test/octave.test/matrix/linspace-5.m
%!test
%! warn_fortran_indexing = 1;
%! fail("linspace ([1, 2; 3, 4], 5, 6)","warning");

