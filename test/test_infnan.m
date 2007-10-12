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

%% test/octave.test/infnan/infnan-1.m
%!test
%! a = Inf;
%! assert(!(finite (a)));

%% test/octave.test/infnan/infnan-2.m
%!test
%! a = Inf;
%! assert(isinf (a));

%% test/octave.test/infnan/infnan-3.m
%!test
%! a = Inf;
%! assert(!(isnan (a)));

%% test/octave.test/infnan/infnan-4.m
%!test
%! b = NaN;
%! assert(!(finite (b)));

%% test/octave.test/infnan/infnan-5.m
%!test
%! b = NaN;
%! assert(!(isinf (b)));

%% test/octave.test/infnan/infnan-6.m
%!test
%! b = NaN;
%! assert(isnan (b));

%% test/octave.test/infnan/infnan-7.m
%!test
%! c = rand ();
%! assert(finite (c));

%% test/octave.test/infnan/infnan-8.m
%!test
%! c = rand ();
%! assert(!(isinf (c)));

%% test/octave.test/infnan/infnan-9.m
%!test
%! c = rand ();
%! assert(!(isnan (c)));

