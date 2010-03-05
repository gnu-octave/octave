## Copyright (C) 2006, 2007, 2008 John W. Eaton
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

%% test/octave.test/logical-wfi-f/s-1.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [];
%! fail("a(0);");
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/s-2.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 2;
%! assert(a(1) == 2);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/s-3.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = 2;
%! assert(a(1) == 2);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/s-4.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%!shared a
%!  a = 2;
%!error id=Octave:index-out-of-bounds a(logical ([1,1]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/v-1.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8,7,6];
%! assert(isempty (a(logical ([0,0,0,0]))));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/v-2.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8,7,6];
%! assert(all (a(logical ([1,1,1,1])) == [9,8,7,6]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/v-3.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8,7,6];
%! assert(all (a(logical ([0,1,1,0])) == [8,7]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/v-4.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8,7,6];
%! assert(all (a(logical ([1,1])) == [9,8]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-1.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(isempty (a(logical ([0,0,0,0]))));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-2.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical ([1,1,1,1])) == [9,7,8,6]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-3.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical ([0,1,1,0])) == [7,8]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-4.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(a(logical (0:1),logical (0:1)) == 6);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-5.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical (0:1),2:-1:1) == [6,7]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-6.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(a(logical (0:1),logical ([0,1])) == 6);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-7.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical (0:1),[2,1]) == [6,7]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-8.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical (0:1),:) == [7,6]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-9.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(a(logical (0:1),1) == 7);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-10.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical (0:1),logical ([1,1])) == [7,6]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-11.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(2:-1:1,logical (0:1)) == [6;8]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-12.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(2:-1:1,logical ([0,1])) == [6;8]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-13.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (all (a(2:-1:1,logical ([1,1])) == [7,6;9,8])));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-14.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(a(logical ([0,1]),logical (0:1)) == 6);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-15.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical ([0,1]),2:-1:1) == [6,7]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-16.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(a(logical ([0,1]),logical ([0,1])) == 6);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-17.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical ([0,1]),[2,1]) == [6,7]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-18.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical ([0,1]),:) == [7,6]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-19.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(a(logical ([0,1]),1) == 7);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-20.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical ([0,1]),logical ([1,1])) == [7,6]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-21.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a([2,1],logical (0:1)) == [6;8]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-22.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a([2,1],logical ([0,1])) == [6;8]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-23.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (all (a([2,1],logical ([1,1])) == [7,6;9,8])));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-24.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(:,logical (0:1)) == [8;6]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-25.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(:,logical ([0,1])) == [8;6]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-26.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (all (a(:,logical ([1,1])) == [9,8;7,6])));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-27.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(a(1,logical (0:1)) == 8);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-28.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(a(1,logical ([0,1])) == 8);
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-29.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(1,logical ([1,1])) == [9,8]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-30.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical ([1,1]),logical (0:1)) == [8;6]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-31.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (all (a(logical ([1,1]),2:-1:1) == [8,9;6,7])));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-32.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical ([1,1]),logical ([0,1])) == [8;6]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-33.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (all (a(logical ([1,1]),[2,1]) == [8,9;6,7])));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-34.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (all (a(logical ([1,1]),:) == [9,8;7,6])));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-35.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (a(logical ([1,1]),1) == [9;7]));
%! warning ("wfi.state", "Octave:fortran-indexing");

%% test/octave.test/logical-wfi-f/m-36.m
%!test
%! wfi = warning ("query", "Octave:fortran-indexing");
%! warning ("off", "Octave:fortran-indexing");
%! a = [9,8;7,6];
%! assert(all (all (a(logical ([1,1]),logical ([1,1])) == [9,8;7,6])));
%! warning ("wfi.state", "Octave:fortran-indexing");

