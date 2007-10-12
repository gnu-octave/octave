## Copyright (C) 2007 John W. Eaton
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

%% test/octave.test/stats/mean-1.m
%!test
%! x = -10:10;
%! y = x';
%! z = [y, y+10];
%! assert(mean (x) == 0 && mean (y) == 0 && mean (z) == [0, 10]);

%% test/octave.test/stats/mean-2.m
%!error mean ();

%% test/octave.test/stats/mean-3.m
%!error mean (1, 2, 3);

%% test/octave.test/stats/median-1.m
%!test
%! x = [1, 2, 3, 4, 5, 6];
%! x2 = x';
%! y = [1, 2, 3, 4, 5, 6, 7];
%! y2 = y';
%! 
%! assert((median (x) == median (x2) && median (x) == 3.5
%! && median (y) == median (y2) && median (y) == 4
%! && median ([x2, 2*x2]) == [3.5, 7]
%! && median ([y2, 3*y2]) == [4, 12]));

%% test/octave.test/stats/median-2.m
%!error median ();

%% test/octave.test/stats/median-3.m
%!error median (1, 2, 3);

%% test/octave.test/stats/std-1.m
%!test
%! x = ones (10, 2);
%! y = [1, 3];
%! assert(std (x) == [0, 0] && abs (std (y) - sqrt (2)) < sqrt (eps));

%% test/octave.test/stats/std-2.m
%!error std ();

%% test/octave.test/stats/std-3.m
%!error std (1, 2, 3, 4);

%% test/octave.test/stats/cov-1.m
%!test
%! x = rand (10);
%! cx1 = cov (x);
%! cx2 = cov (x, x);
%! assert(size (cx1) == [10, 10] && size (cx2) == [10, 10] && cx1 == cx2);

%% test/octave.test/stats/cov-2.m
%!error cov ();

%% test/octave.test/stats/cov-3.m
%!error cov (1, 2, 3);

%% test/octave.test/stats/corrcoef-1.m
%!test
%! x = rand (10);
%! cc1 = corrcoef (x);
%! cc2 = corrcoef (x, x);
%! assert((size (cc1) == [10, 10] && size (cc2) == [10, 10]
%! && abs (cc1 - cc2) < sqrt (eps)));

%% test/octave.test/stats/corrcoef-2.m
%!error corrcoef ();

%% test/octave.test/stats/corrcoef-3.m
%!error corrcoef (1, 2, 3);

%% test/octave.test/stats/kurtosis-1.m
%!test
%! x = [-1; 0; 0; 0; 1];
%! y = [x, 2*x];
%! assert(all (abs (kurtosis (y) - [-1.4, -1.4]) < sqrt (eps)));

%% test/octave.test/stats/kurtosis-2.m
%!error kurtosis ();

%% test/octave.test/stats/kurtosis-3.m
%!error kurtosis (1, 2, 3);

%% test/octave.test/stats/mahalanobis-2.m
%!error mahalanobis ();

%% test/octave.test/stats/mahalanobis-3.m
%!error mahalanobis (1, 2, 3);

%% test/octave.test/stats/skewness-2.m
%!error skewness ();

%% test/octave.test/stats/skewness-3.m
%!error skewness (1, 2, 3);

