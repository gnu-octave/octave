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

%% test/octave.test/poly/compan-1.m
%!assert(all (all (compan ([1, 2, 3]) == [-2, -3; 1, 0])));

%% test/octave.test/poly/compan-2.m
%!assert(all (all (compan ([1; 2; 3]) == [-2, -3; 1, 0])));

%% test/octave.test/poly/compan-3.m
%!assert(isempty (compan (4)));

%% test/octave.test/poly/compan-4.m
%!assert(all (all (compan ([3, 2, 1]) == [-2/3, -1/3; 1, 0])));

%% test/octave.test/poly/compan-5.m
%!error compan ([1,2;3,4]);

%% test/octave.test/poly/compan-6.m
%!error compan ([]);

%% test/octave.test/poly/conv-1.m
%!assert(all (all (conv (ones (3, 1), ones (3, 1)) == [1, 2, 3, 2, 1])));

%% test/octave.test/poly/conv-2.m
%!assert(all (all (conv (ones (1, 3), ones (3, 1)) == [1, 2, 3, 2, 1])));

%% test/octave.test/poly/conv-3.m
%!assert(all (all (conv (3, [1, 2, 3]) == [3, 6, 9])));

%% test/octave.test/poly/conv-4.m
%!error conv ([1, 2; 3, 4], 3);

%% test/octave.test/poly/conv-5.m
%!assert(conv (2, 3),6);

%% test/octave.test/poly/conv-6.m
%!error conv (2, []);

%% test/octave.test/poly/deconv-1.m
%!test
%! [b, r] = deconv ([3, 6, 9, 9], [1, 2, 3]);
%! assert(all (all (b == [3, 0])) && all (all (r == [0, 0, 0, 9])));

%% test/octave.test/poly/deconv-2.m
%!test
%! [b, r] = deconv ([3, 6], [1, 2, 3]);
%! assert(b == 0 && all (all (r == [0, 3, 6])));

%% test/octave.test/poly/deconv-3.m
%!test
%! [b, r] = deconv ([3, 6], [1; 2; 3]);
%! assert(b == 0 && all (all (r == [0, 3, 6])));

%% test/octave.test/poly/deconv-4.m
%!error [b, r] = deconv ([3, 6], [1, 2; 3, 4]);;

%% test/octave.test/poly/deconv-5.m
%!error <number of rows must match> [b, r] = deconv ([3; 6], [1, 2, 3]);

%% test/octave.test/poly/poly-1.m
%!assert(all (all (poly ([1, 2, 3]) == [1, -6, 11, -6])));

%% test/octave.test/poly/poly-2.m
%!assert(all (all (abs (poly ([1, 2; 3, 4]) - [1, -5, -2]) < sqrt (eps))));

%% test/octave.test/poly/poly-3.m
%!error poly ([1, 2, 3; 4, 5, 6]);

%% test/octave.test/poly/poly-4.m
%!assert(poly ([]),1);

%% test/octave.test/poly/polyderiv-1.m
%!assert(all (all (polyderiv ([1, 2, 3]) == [2, 2])));

%% test/octave.test/poly/polyderiv-2.m
%!assert(polyderiv (13) == 0);

%% test/octave.test/poly/polyderiv-3.m
%!error polyderiv ([]);

%% test/octave.test/poly/polyderiv-4.m
%!error polyderiv ([1, 2; 3, 4]);

%% test/octave.test/poly/polyfit-1.m
%!test
%! x = [-2, -1, 0, 1, 2];
%! assert(all (all (abs (polyfit (x, x.^2+x+1, 2) - [1, 1, 1]) < sqrt (eps))));

%% test/octave.test/poly/polyfit-2.m
%!test
%! x = [-2, -1, 0, 1, 2];
%! assert(all (all (abs (polyfit (x, x.^2+x+1, 3) - [0, 1, 1, 1]) < sqrt (eps))));

%% test/octave.test/poly/polyfit-3.m
%!error polyfit ([1, 2; 3, 4], [1, 2; 3, 4], 4);

%% test/octave.test/poly/polyfit-4.m
%!test
%! x = [-2, -1, 0, 1, 2];
%! fail("polyfit (x, x.^2+x+1)");

%% test/octave.test/poly/polyfit-5.m
%!test
%! x = [-2, -1, 0, 1, 2];
%! fail("polyfit (x, x.^2+x+1, [])");

%% test/octave.test/poly/polyinteg-1.m
%!assert(all (all (polyinteg ([2, 2]) == [1, 2, 0])));

%% test/octave.test/poly/polyinteg-2.m
%!assert(isempty (polyinteg ([])));

%% test/octave.test/poly/polyinteg-3.m
%!assert(all (all (polyinteg (3) == [3, 0])));

%% test/octave.test/poly/polyinteg-4.m
%!error polyinteg ([1, 2; 3, 4]);

%% test/octave.test/poly/polyreduce-1.m
%!assert(all (all (polyreduce ([0, 0, 1, 2, 3]) == [1, 2, 3])));

%% test/octave.test/poly/polyreduce-2.m
%!assert(all (all (polyreduce ([1, 2, 3, 0, 0]) == [1, 2, 3, 0, 0])));

%% test/octave.test/poly/polyreduce-3.m
%!assert(all (all (polyreduce ([1, 0, 3]) == [1, 0, 3])));

%% test/octave.test/poly/polyreduce-4.m
%!assert(isempty (polyreduce ([])));

%% test/octave.test/poly/polyreduce-5.m
%!error polyreduce ([1, 2; 3, 4]);

%% test/octave.test/poly/polyval-1.m
%!assert(polyval ([1, 1, 1], 2) == 7);

%% test/octave.test/poly/polyval-2.m
%!assert(all (all (polyval ([1, 1, 1], [0; 1; 2]) == [1; 3; 7])));

%% test/octave.test/poly/polyval-3.m
%!assert(isempty (polyval ([1, 1, 1], [])));

%% test/octave.test/poly/polyval-4.m
%!assert(all (all (polyval ([1, 1, 1], [-1, 0; 1, 2]) == [1, 1; 3, 7])));

%% test/octave.test/poly/polyval-5.m
%!error polyval ([1, 2; 3, 4], [-1, 0; 1, 2]);

%% test/octave.test/poly/polyval-6.m
%!assert(isempty (polyval ([], [-1, 0; 1, 2])));

%% test/octave.test/poly/polyvalm-1.m
%!assert(isempty (polyvalm ([], [1, 2; 3, 4])));

%% test/octave.test/poly/polyvalm-2.m
%!error polyvalm ([1, 1, 1], [1, 2; 3, 4; 5, 6]);

%% test/octave.test/poly/roots-1.m
%!assert(all (all (abs (roots ([1, -6, 11, -6]) - [3; 2; 1]) < sqrt (eps))));

%% test/octave.test/poly/roots-2.m
%!assert(isempty (roots ([])));

%% test/octave.test/poly/roots-3.m
%!error roots ([1, 2; 3, 4]);

