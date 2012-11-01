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

%% test/octave.test/eval-catch/eval-catch-1.m
%!test
%! eval ("clear a; a;", "");

%% test/octave.test/eval-catch/eval-catch-2.m
%!test
%! eval ("", "error ('Should not get here');");

%% test/octave.test/eval-catch/eval-catch-3.m
%!test
%! eval ("clear a; a; x = 0;", "x = 1;");
%! assert (x, 1);

%% test/octave.test/eval-catch/eval-catch-5.m
%!test
%! eval ("clear a; a; str = '';", "str=lasterr;");
%! assert (lasterr()(1:13), "'a' undefined");
%! assert (str(1:13), "'a' undefined");

%% test/octave.test/eval-catch/eval-catch-6.m
%!test
%! eval ("error ('user-defined error'); str = '';", "str = lasterr;");
%! assert (lasterr()(1:18), "user-defined error");
%! assert (str(1:18), "user-defined error");

%% test/octave.test/eval-catch/eval-catch-7.m
%!function ms = mangle (s)
%!  ## Wrap angle brackets around S.
%!  ms = cstrcat ("<", s, ">");
%!endfunction
%!test
%! eval ("clear a; a; str='';", "str = mangle (lasterr);");
%! assert (mangle(lasterr)(1:14), "<'a' undefined");
%! assert (str(1:14), "<'a' undefined");

%% test/octave.test/eval-catch/eval-catch-8.m
%!test
%! eval ("eval (\"clear a; a;str1='';\", \"str1=lasterr;\"); clear b; b; str2='';",
%! "str2 = lasterr;");
%! assert (str1(1:13), "'a' undefined");
%! assert (str2(1:13), "'b' undefined");

%% test/octave.test/eval-catch/eval-catch-9.m
%!test
%! eval ("clear a; a; str1='';",
%! "eval (\"clear b; b; str2='';\", \"str2=lasterr;\"); str1=lasterr;");
%! assert (str1(1:13), "'b' undefined");
%! assert (str2(1:13), "'b' undefined");

%% test/octave.test/eval-catch/eval-catch-10.m
%!test
%! eval ("eval (\"clear a; a; str='';\",\"error (cstrcat (\\\"rethrow: \\\", lasterr));str='';\");",
%! "str=lasterr;");
%! assert (str(1:22), "rethrow: 'a' undefined");

