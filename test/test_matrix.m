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

%% test/octave.test/matrix/diff-1.m
%!assert((diff ([1, 2, 3, 4]) == [1, 1, 1]
%! && diff ([1, 3, 7, 19], 2) == [2, 8]
%! && diff ([1, 2; 5, 4; 8, 7; 9, 6; 3, 1]) == [4, 2; 3, 3; 1, -1; -6, -5]
%! && diff ([1, 2; 5, 4; 8, 7; 9, 6; 3, 1], 3) == [-1, -5; -5, 0]
%! && isempty (diff (1))));

%% test/octave.test/matrix/diff-2.m
%!error diff ([1, 2; 3, 4], -1);

%% test/octave.test/matrix/diff-3.m
%!error diff ("foo");

%% test/octave.test/matrix/diff-4.m
%!error diff ();

%% test/octave.test/matrix/diff-5.m
%!error diff (1, 2, 3, 4);

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

%% test/octave.test/matrix/fliplr-1.m
%!assert((fliplr ([1, 2; 3, 4]) == [2, 1; 4, 3]
%! && fliplr ([1, 2; 3, 4; 5, 6]) == [2, 1; 4, 3; 6, 5]
%! && fliplr ([1, 2, 3; 4, 5, 6]) == [3, 2, 1; 6, 5, 4]));

%% test/octave.test/matrix/fliplr-2.m
%!error <usage.*fliplr> fliplr();

%% test/octave.test/matrix/fliplr-3.m
%!error fliplr (1, 2);

%% test/octave.test/matrix/flipud-1.m
%!assert((flipud ([1, 2; 3, 4]) == [3, 4; 1, 2]
%! && flipud ([1, 2; 3, 4; 5, 6]) == [5, 6; 3, 4; 1, 2]
%! && flipud ([1, 2, 3; 4, 5, 6]) == [4, 5, 6; 1, 2, 3]));

%% test/octave.test/matrix/flipud-2.m
%!error flipud ();

%% test/octave.test/matrix/flipud-3.m
%!error flipud (1, 2);

%% test/octave.test/matrix/rot90-1.m
%!test
%! x1 = [1, 2;
%! 3, 4];
%! x2 = [2, 4;
%! 1, 3];
%! x3 = [4, 3;
%! 2, 1];
%! x4 = [3, 1;
%! 4, 2];
%! 
%! assert((rot90 (x1)== x2 && rot90 (x1, 2) == x3 && rot90 (x1, 3) == x4
%! && rot90 (x1, 4) == x1 && rot90 (x1, 5) == x2 && rot90 (x1, -1) == x4));

%% test/octave.test/matrix/rot90-2.m
%!error rot90 ();

%% test/octave.test/matrix/rot90-3.m
%!error rot90 (1, 2, 3);

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

%% test/octave.test/matrix/shift-1.m
%!test
%! a = [1, 2, 3];
%! b = [4, 5, 6];
%! c = [7, 8, 9];
%! 
%! r = [a, b, c];
%! m = [a; b; c];
%! 
%! assert((shift (r, 3) == [c, a, b]
%! && shift (r, -6) == [c, a, b]
%! && shift (r, -3) == [b, c, a]
%! && shift (m, 1) == [c; a; b]
%! && shift (m, -2) == [c; a; b]));

%% test/octave.test/matrix/shift-2.m
%!error shift ();

%% test/octave.test/matrix/shift-3.m
%!error shift (1, 2, 3, 4);

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

%% test/octave.test/matrix/tril-1.m
%!test
%! a = [1, 2, 3; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%! 
%! l0 = [1, 0, 0; 4, 5, 0; 7, 8, 9; 10, 11, 12];
%! l1 = [1, 2, 0; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%! l2 = [1, 2, 3; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%! lm1 = [0, 0, 0; 4, 0, 0; 7, 8, 0; 10, 11, 12];
%! lm2 = [0, 0, 0; 0, 0, 0; 7, 0, 0; 10, 11, 0];
%! lm3 = [0, 0, 0; 0, 0, 0; 0, 0, 0; 10, 0, 0];
%! lm4 = [0, 0, 0; 0, 0, 0; 0, 0, 0; 0, 0, 0];
%! 
%! assert((tril (a, -4) == lm4 && tril (a, -3) == lm3
%! && tril (a, -2) == lm2 && tril (a, -1) == lm1
%! && tril (a) == l0 && tril (a, 1) == l1 && tril (a, 2) == l2));

%% test/octave.test/matrix/tril-2.m
%!error tril ();

%% test/octave.test/matrix/tril-3.m
%!error tril (1, 2, 3);

%% test/octave.test/matrix/triu-1.m
%!test
%! a = [1, 2, 3; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%! 
%! u0 = [1, 2, 3; 0, 5, 6; 0, 0, 9; 0, 0, 0];
%! u1 = [0, 2, 3; 0, 0, 6; 0, 0, 0; 0, 0, 0];
%! u2 = [0, 0, 3; 0, 0, 0; 0, 0, 0; 0, 0, 0];
%! u3 = [0, 0, 0; 0, 0, 0; 0, 0, 0; 0, 0, 0];
%! um1 = [1, 2, 3; 4, 5, 6; 0, 8, 9; 0, 0, 12];
%! um2 = [1, 2, 3; 4, 5, 6; 7, 8, 9; 0, 11, 12];
%! um3 = [1, 2, 3; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%! 
%! assert((triu (a, -3) == um3 && triu (a, -2) == um2
%! && triu (a, -1) == um1 && triu (a) == u0 && triu (a, 1) == u1
%! && triu (a, 2) == u2 && triu (a, 3) == u3));

%% test/octave.test/matrix/triu-2.m
%!error triu ();

%% test/octave.test/matrix/triu-3.m
%!error triu (1, 2, 3);

%% test/octave.test/matrix/vec-1.m
%!assert(vec ([1, 2; 3, 4]) == [1; 3; 2; 4] && vec ([1, 3, 2, 4]) == [1; 3; 2; 4]);

%% test/octave.test/matrix/vec-2.m
%!error vec ();

%% test/octave.test/matrix/vec-3.m
%!error vec (1, 2);

%% test/octave.test/matrix/vech-1.m
%!assert(all (vech ([1, 2, 3; 4, 5, 6; 7, 8, 9]) == [1; 4; 7; 5; 8; 9]));

%% test/octave.test/matrix/vech-2.m
%!error vech ();

%% test/octave.test/matrix/vech-3.m
%!error vech (1, 2);

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

%% test/octave.test/matrix/logspace-1.m
%!test
%! x1 = logspace (1, 2);
%! x2 = logspace (1, 2, 10);
%! x3 = logspace (1, -2, 10);
%! x4 = logspace (1, pi, 10);
%! assert((size (x1) == [1, 50] && x1(1) == 10 && x1(50) == 100
%! && size (x2) == [1, 10] && x2(1) == 10 && x2(10) == 100
%! && size (x3) == [1, 10] && x3(1) == 10 && x3(10) == 0.01
%! && size (x4) == [1, 10] && x4(1) == 10 && abs (x4(10) - pi) < sqrt (eps)));

%% test/octave.test/matrix/logspace-2.m
%!error logspace ([1, 2; 3, 4], 5, 6);

%% test/octave.test/matrix/logspace-3.m
%!error logspace ();

%% test/octave.test/matrix/logspace-4.m
%!error logspace (1, 2, 3, 4);

%% test/octave.test/matrix/sylvester_matrix-1.m
%!assert((sylvester_matrix (1) == [1, 1; 1, -1]
%! && (sylvester_matrix (2)
%! == [1, 1, 1, 1; 1, -1, 1, -1; 1, 1, -1, -1; 1, -1, -1, 1])));

%% test/octave.test/matrix/sylvester_matrix-2.m
%!error sylvester_matrix ([1, 2; 3, 4]);

%% test/octave.test/matrix/sylvester_matrix-3.m
%!error sylvester_matrix ();

%% test/octave.test/matrix/sylvester_matrix-4.m
%!error sylvester_matrix (1, 2);

%% test/octave.test/matrix/hankel-1.m
%!assert((hankel (1) == 1 && hankel ([1, 2]) == [1, 2; 2, 0]
%! && hankel ([1, 2], [2; -1; -3]) == [1, 2, -1; 2, -1, -3]));

%% test/octave.test/matrix/hankel-2.m
%!error hankel ([1, 2; 3, 4], [1, 2; 3, 4]);

%% test/octave.test/matrix/hankel-3.m
%!error hankel ();

%% test/octave.test/matrix/hankel-4.m
%!error hankel (1, 2, 3);

%% test/octave.test/matrix/hilb-1.m
%!assert((hilb (2) == [1, 1/2; 1/2, 1/3]
%! && hilb (3) == [1, 1/2, 1/3; 1/2, 1/3, 1/4; 1/3, 1/4, 1/5]));

%% test/octave.test/matrix/hilb-2.m
%!error hilb ();

%% test/octave.test/matrix/hilb-3.m
%!error hilb (1, 2);

%% test/octave.test/matrix/invhilb-1.m
%!test
%! result4 = [16, -120, 240, -140;
%! -120, 1200, -2700, 1680;
%! 240, -2700, 6480, -4200;
%! -140, 1680, -4200, 2800];
%! 
%! assert((invhilb (1) == 1 && invhilb (2) == [4, -6; -6, 12]
%! && invhilb (4) == result4
%! && abs (invhilb (7) * hilb (7) - eye (7)) < sqrt (eps)));

%% test/octave.test/matrix/invhilb-2.m
%!error invhilb ([1, 2]);

%% test/octave.test/matrix/invhilb-3.m
%!error invhilb ();

%% test/octave.test/matrix/invhilb-4.m
%!error invhilb (1, 2);

%% test/octave.test/matrix/toeplitz-1.m
%!assert((toeplitz (1) == 1
%! && toeplitz ([1, 2, 3], [1; -3; -5]) == [1, -3, -5; 2, 1, -3; 3, 2, 1]
%! && toeplitz ([1, 2, 3], [1; -3i; -5i]) == [1, -3i, -5i; 2, 1, -3i; 3, 2, 1]));

%% test/octave.test/matrix/toeplitz-2.m
%!error toeplitz ([1, 2; 3, 4], 1);

%% test/octave.test/matrix/toeplitz-3.m
%!error toeplitz ();

%% test/octave.test/matrix/toeplitz-4.m
%!error toeplitz (1, 2, 3);

%% test/octave.test/matrix/vander-1.m
%!assert((vander (1) == 1 && vander ([1, 2, 3]) == vander ([1; 2; 3])
%! && vander ([1, 2, 3]) == [1, 1, 1; 4, 2, 1; 9, 3, 1]
%! && vander ([1, 2, 3]*i) == [-1, i, 1; -4, 2i, 1; -9, 3i, 1]));

%% test/octave.test/matrix/vander-2.m
%!error vander ([1, 2; 3, 4]);

%% test/octave.test/matrix/vander-3.m
%!error vander ();

%% test/octave.test/matrix/vander-4.m
%!error vander (1, 2);

