## Copyright (C) 2010-2011 John W. Eaton
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

## Tests for parser problems belong in this file.  We need many more
## tests here!

%!assert ({1 2 {3 4}}, {1,2,{3,4}})
%!assert ({1, 2 {3 4}}, {1,2,{3,4}})
%!assert ({1 2, {3 4}}, {1,2,{3,4}})
%!assert ({1 2 {3, 4}}, {1,2,{3,4}})
%!assert ({1, 2, {3 4}}, {1,2,{3,4}})
%!assert ({1 2,{3 4}}, {1,2,{3,4}})
%!assert ({1 2,{3,4}}, {1,2,{3,4}})
%!assert ({1,2,{3 4}}, {1,2,{3,4}})

%# Tests for operator precedence as documented in section 8.8 of manual
%# There are 11 levels of precedence from "exponentiation" (highest) down to
%# "statement operators" (lowest).
%#
%# Level 11 (exponentiation) overrides all others
%!test
%!  assert (-2^2, -4)
%!  assert (!0^0, false);
# FIXME: This test is failing.  Transpose mistakenly has higher priority.
%!#  assert ([2 3].^2', [4; 9])
%!  assert (2*3^2, 18)
%!  assert (2+3^2, 11)
%!  assert ([1:10](1:2^2), [1 2 3 4])
%!  assert (3 > 2^2, false)
%!  assert (1 & 0^0, true)
%!  assert (1 && 0^0, true)
%!  a = 3;
%!  a *= 0^0;
%!  assert (a, 3)
%# Level 10 (unary plus, increment, not)
%!test
# FIXME: No test for increment and transpose that I can think of.
%!  a = 2;
%!  assert (++a*3, 9)
%!  assert (a++-2, 1)
%!  assert (a, 4)
%!  assert ([1:10](1:++a), [1:5])
%!  assert (5 == a++, true)
%!  assert (7 == ++a, true)
%!  a = 0;
%!  assert (1 & a++, false)
%!  assert (a, 1)
%!  assert (1 && --a, false)
%!  a = 3;
%!  a *= a++;
%!  assert (a, 12)
%# Level 9 (transpose)
%!test
%!  assert ([1 2]*[3 4]', 11)
%!  assert ([1 2]'+[3 4]', [4; 6])
%!  assert (1:5', 1:5)
%!  assert ([1; 2] == [1 2]', [true; true])
%!  assert ([1; 0] & [1 0]', [true; false])
# FIXME: No test for transpose and short-circuit operator that I can think of.
%!  a = [1 2];
%!  a *= [3 4]';
%!  assert (a, 11)
%# Level 8 (multiply, divide)
%!test
%!  assert (3 + 4 * 5, 23)
%!  assert (3 + 4 * 5, 23)
%!  assert (5*1:6, [5 6])
%!  assert (3 > 1 * 5, false)
%!  assert (1 & 1 * 0, false)
%!  assert (1 && 1 * 0, false)
%!  a = 3;
%!  a /= a * 2;
%!  assert (a, 0.5)
%# Level 7 (add, subtract)
%!test
%!  assert ([2 + 1:6], 3:6)
%!  assert (3 > 1 + 5, false)
%!  assert (1 & 1 - 1, false)
%!  assert (1 && 1 - 1, false)
%!  a = 3;
%!  a *= 1 + 1;
%!  assert (a, 6)
%# Level 6 (colon)
%!test
%!  assert (5:-1: 3 > 4, [true false false])
%!  assert (1: 3 & 1, [true true true])
%!  assert (-1: 3 && 1, false)
%!  a = [1:3];
%!  a += 3 : 5;
%!  assert (a, [4 6 8])
%# Level 5 (relational)
%!test
%!  assert (0 == -1 & 0, false)
%!  assert (0 == -1 && 0, false)
%!  a = 2;
%!  a *= 3 > 1;
%!  assert (a, 2)
%# Level 4 (element-wise and, or)
%!test
%!  assert (0 & 1 || 1, true)
%!  assert (0 == -1 && 0, false)
%!  a = 2;
%!  a *= 3 & 1;
%!  assert (a, 2)
%# Level 3 (logical and, or)
%!test
%!  a = 2;
%!  a *= 3 && 1;
%!  assert (a, 2)

%# Tests for operator precedence within each level where ordering should
%# be left to right except for exponents and assignments.
%# Level 11 (exponentiation)
%!test
%# FIXME : Exponentiation seems to work left to right, despite the 
%#         documentation and ordinary mathematical rules of precedence.
%!#  assert (2^3**2, 512)
%# Level 10 (unary plus, increment, not)
%!test
%!  assert (+-+1, -1)
%!  a = 0;
%# FIXME : Should we test for this corner case at all?
%#         (unary minus)(auto-decrement operator)
%!#  assert (---a, 1);
%!  a = -1;
%!  assert (!++a, true)
%!  assert (a, 0)
%!  assert (-~a, -1)
%!  assert (!~a++, false)
%!  assert (a, 1)
%# Level 9 (transpose)
%!test
%!  assert (3*4i'.', 0 - 12i)
%!  assert (3*4i.'.', 0 + 12i)
%# Level 8 (multiply, divide)
%!test
%!assert (3 * 4 / 5, 2.4)
%!assert (3 ./ 4 .* 5, 3.75)
%# Level 7 (add, subtract)
%!test
%!assert (-3 - 4 + 1 + 3 * 2, 0)
%# Level 5 (relational)
%!test
%!  assert (0 < 1 <= 0.5 == 0 >= 0.5 > 0, true)
%!  assert (1 < 1 == 0 != 0, true)
%!  assert (1 < 1 == 0 ~= 0, true)
%# Level 4 (element-wise and, or)
%!test
%!  assert ([ 1 0] & [0 1] | [1 0], [true false])
%# Level 2 (assignment)
%!test
%! a = 2; b = 5; c = 7;
%! assert (a += b *= c += 1, 42)
%! assert (b == 40 && c == 8)
