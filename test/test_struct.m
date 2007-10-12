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

%% test/octave.test/struct/fieldnames-1.m
%!test
%! s.a = 1;
%! c = fieldnames (s);
%! assert(iscell (c) && strcmp (c{1}, "a"));

%% test/octave.test/struct/fieldnames-2.m
%!test
%! s.a.b = 1;
%! c = fieldnames (s.a);
%! assert(iscell (c) && strcmp (c{1}, "b"));

%% test/octave.test/struct/fieldnames-3.m
%!error <Invalid call to fieldnames.*> fieldnames ();

%% test/octave.test/struct/fieldnames-4.m
%!test
%! s.a = 1;
%! fail("fieldnames (s, 1)","Invalid call to fieldnames.*");

%% test/octave.test/struct/fieldnames-5.m
%!error fieldnames (1);

%% test/octave.test/struct/isfield-1.m
%!test
%! s.aaa = 1;
%! s.a = 2;
%! assert(isfield (s, "a"));

%% test/octave.test/struct/isfield-2.m
%!test
%! s.aaa = 1;
%! s.a = 2;
%! assert(!(isfield (s, "b")));

%% test/octave.test/struct/isfield-3.m
%!error <Invalid call to isfield.*> isfield ();

%% test/octave.test/struct/isfield-4.m
%!test
%! s.aaa = 1;
%! s.a = 2;
%! fail("isfield (s, 'a', 3);","Invalid call to isfield.*");

%% test/octave.test/struct/isfield-5.m
%!assert(isfield (1, "m") == 0);

%% test/octave.test/struct/isfield-6.m
%!test
%! s.a = 2;
%! assert(isfield (s, 2) == 0);

%% test/octave.test/struct/isstruct-1.m
%!assert(!(isstruct (1)));

%% test/octave.test/struct/isstruct-2.m
%!assert(!(isstruct ([1, 2])));

%% test/octave.test/struct/isstruct-3.m
%!assert(!(isstruct ([])));

%% test/octave.test/struct/isstruct-4.m
%!assert(!(isstruct ([1, 2; 3, 4])));

%% test/octave.test/struct/isstruct-5.m
%!assert(!(isstruct ("t")));

%% test/octave.test/struct/isstruct-6.m
%!assert(!(isstruct ("test")));

%% test/octave.test/struct/isstruct-7.m
%!assert(!(isstruct (["test"; "ing"])));

%% test/octave.test/struct/isstruct-8.m
%!test
%! s.a = 1;
%! assert(isstruct (s));

%% test/octave.test/struct/isstruct-9.m
%!test
%! s.a.b = 1;
%! assert(isstruct (s.a));

%% test/octave.test/struct/isstruct-10.m
%!error <Invalid call to isstruct.*> isstruct ();

%% test/octave.test/struct/isstruct-11.m
%!test
%! s.a = 1;
%! fail("isstruct (s, 1)","Invalid call to isstruct.*");

