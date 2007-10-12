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

%% test/octave.test/eval/eval-1.m
%!#test
%! x = 1;
%! assert(eval ("x"),1);

%% test/octave.test/eval/eval-2.m
%!test
%! x = 1;
%! assert(eval ("x;"));

%% test/octave.test/eval/eval-3.m
%!test
%! x = 1;
%! assert(eval ("x;"),1);

%% FIXME
%% Disable this test as adding the ";" is redundant with eval-1 and
%% in any case is a syntax error with assert
%% test/octave.test/eval/eval-4.m
%!#test
%! x = 1;
%! assert(eval ("x");,1);

%% test/octave.test/eval/eval-5.m
%!test
%! eval ("flipud = 2;");
%! assert(flipud,2);

%% test/octave.test/eval/eval-6.m
%!function y = f ()
%!  eval ("flipud = 2;");
%!  y = flipud;
%!test
%! assert(f,2);

%% test/octave.test/eval/eval-7.m
%!#test
%! eval ("x = 1");
%! assert(x,1);

%% test/octave.test/eval/eval-8.m
%!test
%! eval ("x = 1;")
%! assert(x,1);

%% test/octave.test/eval/eval-9.m
%!test
%! eval ("x = 1;");
%! assert(x,1);

%% test/octave.test/eval/eval-10.m
%!#test
%! eval ("x = 1")
%! assert(x,1);

%% test/octave.test/eval/eval-11.m
%!test
%! x = 1;
%! y = eval ("x");
%! assert(y,1);

%% test/octave.test/eval/eval-12.m
%!test
%! x = 1;
%! y = eval ("x;");
%! assert(y,1);

%% test/octave.test/eval/eval-13.m
%!test
%! x = 1;
%! y = eval ("x;");
%! assert(y,1);

%% test/octave.test/eval/eval-14.m
%!test
%! x = 1;
%! y = eval ("x");
%! assert(y,1);

