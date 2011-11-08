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

%% test/octave.test/logical-wfi-t/s-1.m
%!test
%! a = [];
%! fail("a(0);");

%% test/octave.test/logical-wfi-t/s-2.m
%!test
%! a = 2;
%! assert(a(1) == 2);

%% test/octave.test/logical-wfi-t/s-3.m
%!test
%! a = 2;
%! assert(a(1) == 2);

%% test/octave.test/logical-wfi-t/s-4.m
%!test
%!shared a
%! a = 2;
%!error id=Octave:index-out-of-bounds a(logical ([1,1]));

%% test/octave.test/logical-wfi-t/v-1.m
%!test
%! a = [9,8,7,6];
%! assert(isempty (a(logical ([0,0,0,0]))));

%% test/octave.test/logical-wfi-t/v-2.m
%!test
%! a = [9,8,7,6];
%! assert(all (a(logical ([1,1,1,1])) == [9,8,7,6]));

%% test/octave.test/logical-wfi-t/v-3.m
%!test
%! a = [9,8,7,6];
%! assert(all (a(logical ([0,1,1,0])) == [8,7]));

%% test/octave.test/logical-wfi-t/v-4.m
%!test
%! a = [9,8,7,6];
%! assert(all (a(logical ([1,1])) == [9,8]));

%% test/octave.test/logical-wfi-t/m-1.m
%!test
%! a = [9,8;7,6];
%! isempty (a(logical ([0,0,0,0])));

%% test/octave.test/logical-wfi-t/m-2.m
%!test
%! a = [9,8;7,6];
%! all (a(logical ([1,1,1,1])) == [9,7,8,6]);

%% test/octave.test/logical-wfi-t/m-3.m
%!test
%! a = [9,8;7,6];
%! all (a(logical ([0,1,1,0])) == [7,8]);

%% test/octave.test/logical-wfi-t/m-4.m
%!test
%! a = [9,8;7,6];
%! assert(a(logical (0:1),logical (0:1)) == 6);

%% test/octave.test/logical-wfi-t/m-5.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(logical (0:1),2:-1:1) == [6,7]));

%% test/octave.test/logical-wfi-t/m-6.m
%!test
%! a = [9,8;7,6];
%! assert(a(logical (0:1),logical ([0,1])) == 6);

%% test/octave.test/logical-wfi-t/m-7.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(logical (0:1),[2,1]) == [6,7]));

%% test/octave.test/logical-wfi-t/m-8.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(logical (0:1),:) == [7,6]));

%% test/octave.test/logical-wfi-t/m-9.m
%!test
%! a = [9,8;7,6];
%! assert(a(logical (0:1),1) == 7);

%% test/octave.test/logical-wfi-t/m-10.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(logical (0:1),logical ([1,1])) == [7,6]));

%% test/octave.test/logical-wfi-t/m-11.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(2:-1:1,logical (0:1)) == [6;8]));

%% test/octave.test/logical-wfi-t/m-12.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(2:-1:1,logical ([0,1])) == [6;8]));

%% test/octave.test/logical-wfi-t/m-13.m
%!test
%! a = [9,8;7,6];
%! assert(all (all (a(2:-1:1,logical ([1,1])) == [7,6;9,8])));

%% test/octave.test/logical-wfi-t/m-14.m
%!test
%! a = [9,8;7,6];
%! assert(a(logical ([0,1]),logical (0:1)) == 6);

%% test/octave.test/logical-wfi-t/m-15.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(logical ([0,1]),2:-1:1) == [6,7]));

%% test/octave.test/logical-wfi-t/m-16.m
%!test
%! a = [9,8;7,6];
%! assert(a(logical ([0,1]),logical ([0,1])) == 6);

%% test/octave.test/logical-wfi-t/m-17.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(logical ([0,1]),[2,1]) == [6,7]));

%% test/octave.test/logical-wfi-t/m-18.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(logical ([0,1]),:) == [7,6]));

%% test/octave.test/logical-wfi-t/m-19.m
%!test
%! a = [9,8;7,6];
%! assert(a(logical ([0,1]),1) == 7);

%% test/octave.test/logical-wfi-t/m-20.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(logical ([0,1]),logical ([1,1])) == [7,6]));

%% test/octave.test/logical-wfi-t/m-21.m
%!test
%! a = [9,8;7,6];
%! assert(all (a([2,1],logical (0:1)) == [6;8]));

%% test/octave.test/logical-wfi-t/m-22.m
%!test
%! a = [9,8;7,6];
%! assert(all (a([2,1],logical ([0,1])) == [6;8]));

%% test/octave.test/logical-wfi-t/m-23.m
%!test
%! a = [9,8;7,6];
%! assert(all (all (a([2,1],logical ([1,1])) == [7,6;9,8])));

%% test/octave.test/logical-wfi-t/m-24.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(:,logical (0:1)) == [8;6]));

%% test/octave.test/logical-wfi-t/m-25.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(:,logical ([0,1])) == [8;6]));

%% test/octave.test/logical-wfi-t/m-26.m
%!test
%! a = [9,8;7,6];
%! assert(all (all (a(:,logical ([1,1])) == [9,8;7,6])));

%% test/octave.test/logical-wfi-t/m-27.m
%!test
%! a = [9,8;7,6];
%! assert(a(1,logical (0:1)) == 8);

%% test/octave.test/logical-wfi-t/m-28.m
%!test
%! a = [9,8;7,6];
%! assert(a(1,logical ([0,1])) == 8);

%% test/octave.test/logical-wfi-t/m-29.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(1,logical ([1,1])) == [9,8]));

%% test/octave.test/logical-wfi-t/m-30.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(logical ([1,1]),logical (0:1)) == [8;6]));

%% test/octave.test/logical-wfi-t/m-31.m
%!test
%! a = [9,8;7,6];
%! assert(all (all (a(logical ([1,1]),2:-1:1) == [8,9;6,7])));

%% test/octave.test/logical-wfi-t/m-32.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(logical ([1,1]),logical ([0,1])) == [8;6]));

%% test/octave.test/logical-wfi-t/m-33.m
%!test
%! a = [9,8;7,6];
%! assert(all (all (a(logical ([1,1]),[2,1]) == [8,9;6,7])));

%% test/octave.test/logical-wfi-t/m-34.m
%!test
%! a = [9,8;7,6];
%! assert(all (all (a(logical ([1,1]),:) == [9,8;7,6])));

%% test/octave.test/logical-wfi-t/m-35.m
%!test
%! a = [9,8;7,6];
%! assert(all (a(logical ([1,1]),1) == [9;7]));

%% test/octave.test/logical-wfi-t/m-36.m
%!test
%! a = [9,8;7,6];
%! assert(all (all (a(logical ([1,1]),logical ([1,1])) == [9,8;7,6])));

