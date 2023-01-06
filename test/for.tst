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
%! for i = 1
%!   __printf_assert__ ("%d", i);
%! end  # "end" is part of test, check not using "endfor"
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("1"));

%!test
%! for i = 1:4
%!   __printf_assert__ ("%d", i);
%! endfor
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("1234"));

%!test
%! for i = [1,2,3,4]
%!   __printf_assert__ ("%d", i);
%! endfor
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("1234"));

%!test
%! for i = [1,2;3,4]
%!   __printf_assert__ ("%d", i(1,1));
%!   __printf_assert__ ("%d", i(2,1));
%! endfor
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("1324"));

%!test
%! for i = I
%!   __printf_assert__ ("%d", imag (i));
%! endfor
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("1"));

%!test
%! for i = [1,2,3,4]*I
%!   __printf_assert__ ("%d", imag (i));
%! endfor
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("1234"));

%!test
%! for i = [1,2;3,4]*I
%!   __printf_assert__ ("%d", imag (i(1,1)));
%!   __printf_assert__ ("%d", imag (i(2,1)));
%! endfor
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("1324"));

%!test
%! for i = [1,2,3,4]
%!   if (i > 2)
%!     break;
%!   endif
%!   __printf_assert__ ("%d", i);
%! endfor
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("12"));

%!test
%! for i = [1,2,3,4]
%!   if (i < 3)
%!     continue;
%!   endif
%!   __printf_assert__ ("%d", i);
%! endfor
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("34"));

%!test
%! a = [1,3;2,4];
%! j = 0;
%! for i = cat (3, a, 4 + a)
%!   assert (i, [1;2] + 2*j++);
%! endfor

%!test
%! a = {1,3;2,4};
%! j = 0;
%! for i = cat (3, a, cellfun (@(x) 4 + x, a, "UniformOutput", 0))
%!   assert (i, {1 + 2*j; 2 + 2*j++});
%! endfor

## test parsing of single-quoted character string appearing at the
## beginning of a for loop
%!test
%! for i = 1:5
%!   'foo';
%! endfor
%! assert (i, 5);

%!test
%! parfor i = 1
%!   __printf_assert__ ("%d", i);
%! end  # "end" is part of test, check not using "endparfor"
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("1"));

%!test
%! parfor i = 1:4
%!   __printf_assert__ ("%d", i);
%! endparfor
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("1234"));

%!test <*50893>
%! cnt = 0;
%! for k = zeros (0,3)
%!   cnt++;
%! endfor
%! assert (cnt, 0);
%! assert (k, zeros (0,3));

%!test <*50893>
%! cnt = 0;
%! for k = zeros (3,0)
%!   cnt++;
%! endfor
%! assert (cnt, 0);
%! assert (k, zeros (3,0));

%!test <*50893>
%! cnt = 0;
%! for k = zeros (3,0, "uint32")
%!   cnt++;
%! endfor
%! assert (cnt, 0);
%! assert (k, zeros (3,0, "uint32"));

%!test <*50893>
%! cnt = 0;
%! for k = cell (0,3)
%!   cnt++;
%! endfor
%! assert (cnt, 0);
%! assert (k, cell (0,3));

%!test <*45143>
%! warning ("on", "Octave:infinite-loop", "local");
%! fail ("for i = 0:inf; break; end", "warning",
%!       "FOR loop limit is infinite");
%!
%! fail ("for i = 0:-1:-inf; break; end", "warning",
%!       "FOR loop limit is infinite");

%!test <*45143>
%! warning ("on", "Octave:infinite-loop", "local");
%! k = 0;
%! for i = 1:Inf
%!   if (++k > 10)
%!     break;
%!   endif
%! endfor
%! assert (i, 11);
%!
%! k = 0;
%! for i = -1:-1:-Inf
%!   if (++k > 10)
%!     break;
%!   endif
%! endfor
%! assert (i, -11);
%!
%! k = 0;
%! for i = 1:-Inf
%!   if (++k > 10)
%!     break;
%!   endif
%! endfor
%! assert (i, zeros (1,0));
%!
%! k = 0;
%! for i = 0:-1:Inf
%!   if (++k > 10)
%!     break;
%!   endif
%! endfor
%! assert (i, zeros (1,0));
