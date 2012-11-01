## Copyright (C) 2006-2012 John W. Eaton
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

%% test/octave.test/try/try-1.m
%!test
%! try
%! catch
%!   error ("Shoudn't get here");
%! end_try_catch

%% test/octave.test/try/try-2.m
%!test
%! try
%!   clear a;
%!   a;
%! catch
%! end_try_catch
%! a = 1;
%! assert (a,1);

%% test/octave.test/try/try-3.m
%!test
%! clear x;
%! try
%!   clear a;
%!   a;
%!   x = 1;
%! catch
%! end_try_catch
%! a = 2;
%! assert (!exist ('x'));
%! assert (a,2);

%% test/octave.test/try/try-4.m
%!test
%! try
%!   clear a;
%!   a;
%! catch
%!   x = 1;
%! end_try_catch
%! assert (exist ('x'));

%% test/octave.test/try/try-5.m
%!test
%! try
%!   clear a;
%!   a;
%!   error ("Shoudn't get here");
%! catch
%!   assert (lasterr()(1:13), "'a' undefined");
%! end_try_catch
%! assert (lasterr()(1:13), "'a' undefined");

%% test/octave.test/try/try-6.m
%!test 
%! try
%!   error ("user-defined error");
%! catch
%!   assert (lasterr, "user-defined error");
%! end_try_catch

%% test/octave.test/try/try-7.m
%!function ms = mangle (s)
%!  ## Wrap angle brackets around S.
%!  ms = cstrcat ("<", s, ">");
%!endfunction
%!test
%! try
%!   clear a;
%!   a;
%!   error ("Shoudn't get here");
%! catch
%!   assert (mangle (lasterr)(1:14), "<'a' undefined");
%! end_try_catch

%% test/octave.test/try/try-8.m
%!test
%! try
%!   try
%!     clear a;
%!     a;
%!     error ("Shoudn't get here");
%!   catch
%!     assert (lasterr()(1:13), "'a' undefined");
%!   end_try_catch
%!   clear b;
%!   b;
%!   error ("Shoudn't get here");
%! catch
%!   assert (lasterr()(1:13), "'b' undefined");
%! end_try_catch

%% test/octave.test/try/try-9.m
%!test
%! try
%!   clear a;
%!   a;
%!   error ("Shoudn't get here");
%! catch
%!   try
%!     assert (lasterr()(1:13), "'a' undefined");
%!     clear b;
%!     b;
%!     error ("Shoudn't get here");
%!   catch
%!     assert (lasterr()(1:13), "'b' undefined");
%!   end_try_catch
%! end_try_catch

%% test/octave.test/try/try-10.m
%!test
%! try
%!   try
%!     clear a;
%!     a;
%!     error ("Shoudn't get here");
%!   catch
%!     error (cstrcat ("rethrow: ", lasterr));
%!   end_try_catch
%! catch
%!   assert (lasterr()(1:22), "rethrow: 'a' undefined");
%! end_try_catch

