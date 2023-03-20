########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

%!test
%! i = 0;
%! if (i == 0)
%!   i++;
%!   __printf_assert__ ("%d\n", i);
%! end  # "end" is part of test, check not using "endif"
%! assert (__prog_output_assert__ ("1"));

%!test
%! if (eye (2))
%!   __printf_assert__ ("fail\n");
%! else
%!   __printf_assert__ ("pass\n");
%! endif
%! assert (__prog_output_assert__ ("pass"));

%!test
%! x = 2;
%! if (eye (2))
%!   __printf_assert__ ("fail\n");
%! elseif (x)
%!   __printf_assert__ ("pass\n");
%! endif
%! assert (__prog_output_assert__ ("pass"));

%!test <63935>
%! if (true (4, 1) & true (4, 1))
%!   __printf_assert__ ("pass\n");
%! elseif (x)
%!   __printf_assert__ ("fail\n");
%! endif
%! assert (__prog_output_assert__ ("pass"));

%!test
%! x = 0;
%! y = -2;
%! if (eye (2))
%!   __printf_assert__ ("fail\n");
%! elseif (x)
%!   __printf_assert__ ("fail\n");
%! elseif (y)
%!   __printf_assert__ ("pass\n");
%! endif
%! assert (__prog_output_assert__ ("pass"));

%!test
%! x = 0;
%! y = -2;
%! if (eye (2))
%!   __printf_assert__ ("fail\n");
%! elseif (x)
%!   __printf_assert__ ("fail\n");
%! elseif (x)
%!   __printf_assert__ ("fail\n");
%! else
%!   __printf_assert__ ("pass\n");
%! endif
%! assert (__prog_output_assert__ ("pass"));

%!test
%! x = 0;
%! y = -2;
%! if (y)
%!   __printf_assert__ ("pass\n");
%! elseif (x)
%!   __printf_assert__ ("fail\n");
%! elseif (x)
%!   __printf_assert__ ("fail\n");
%! endif
%! assert (__prog_output_assert__ ("pass"));

## test parsing of single-quoted character string appearing at the
## beginning of an if condition
%!test
%! if (1)
%!   'foo';
%!   x = 13;
%! endif
%! assert (x, 13);

## test parsing of single-quoted character string appearing at the
## beginning of an if condition
%!test
%! if (0)
%!   x = 42;
%! elseif (1)
%!   'foo';
%!   x = 13;
%! endif
%! assert (x, 13);

## test "is_true" of different data types
%!error diag (NaN) || 0
%!test
%! d1 = diag ([])    || 0;
%! d2 = diag (1)     || 0;
%! d3 = diag ([1 2]) || 0;
%! assert ([d1 d2 d3], [false true false]);

%!error sparse (NaN) || 0
%!error sparse ([1 1 ; 1 NaN]) || 0
%!test
%! s1 = sparse ([])  || 0;
%! s2 = sparse (1)   || 0;
%! s3 = sparse ([1 0 ; 0 2])   || 0;
%! s4 = sparse ([1 1 ; 1 1])   || 0;
%! assert ([s1 s2 s3 s4], [false true false true]);

%!test
%! r1 = (1:10) || 0;
%! r2 = (-10:-1) || 0;
%! r3 = (-1:1) || 0;
%! assert ([r1 r2 r3], [true true false]);

%!test
%! c1 = [2i 4i] || 0;
%! c2 = [22 4i] || 0;
%! c3 = i || 0;
%! c4 = complex (0) || 0;
%! assert ([c1 c2 c3 c4], [true true true false]);
