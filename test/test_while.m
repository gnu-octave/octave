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

%% test/octave.test/while/while-1.m
%!test
%! i = 0;
%! while (eye (2))
%! i++;
%! printf_assert ("%d\n", i);
%! endwhile;
%! assert(prog_output_assert(""));

%% test/octave.test/while/while-2.m
%!test
%! i = 5;
%! while (--i)
%! printf_assert ("%d", i);
%! endwhile
%! printf_assert ("\n");
%! assert(prog_output_assert("4321"));

%% test/octave.test/while/while-3.m
%!test
%! i = 5;
%! while (i)
%! i--;
%! printf_assert ("%d", i);
%! endwhile
%! printf_assert ("\n");
%! assert(prog_output_assert("43210"));

%% test/octave.test/while/while-4.m
%!test
%! i = 0;
%! while (i++ < 20)
%! if (i > 2)
%! break;
%! endif
%! printf_assert ("%d", i);
%! endwhile;
%! printf_assert ("\n");
%! assert(prog_output_assert("12"));

%% test/octave.test/while/while-5.m
%!test
%! i = 0;
%! while (++i < 5)
%! if (i < 3)
%! continue;
%! endif
%! printf_assert ("%d", i);
%! endwhile
%! printf_assert ("\n");
%! assert(prog_output_assert("34"));

