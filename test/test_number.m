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

%% test/octave.test/number/ismatrix-1.m
%!assert(ismatrix (1));

%% test/octave.test/number/ismatrix-2.m
%!assert(ismatrix ([1, 2, 3]));

%% test/octave.test/number/ismatrix-3.m
%% Yes, this is right, ismatrix() checks for non-empty matrices.
%!assert(
%! ismatrix ([]) == 0);

%% test/octave.test/number/ismatrix-4.m
%!assert(ismatrix ([1, 2; 3, 4]));

%% test/octave.test/number/ismatrix-5.m
%!test
%! warn_str_to_num = 0;
%! assert(!(ismatrix ("t")));

%% test/octave.test/number/ismatrix-6.m
%!test
%! warn_str_to_num = 0;
%! assert(!(ismatrix ("test")));

%% test/octave.test/number/ismatrix-7.m
%!test
%! warn_str_to_num = 0;
%! assert(!(ismatrix (["test"; "ing"])));

%% test/octave.test/number/ismatrix-8.m
%!test
%! s.a = 1;
%! assert(!(ismatrix (s)));

%% test/octave.test/number/ismatrix-9.m
%!error <Invalid call to ismatrix.*> ismatrix ();

%% test/octave.test/number/ismatrix-10.m
%!error <Invalid call to ismatrix.*> ismatrix ([1, 2; 3, 4], 2);

