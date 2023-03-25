########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
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

## Tests for parser problems belong in this file.
## We need many more tests here!

## Test cell construction operator {}
%!assert ({1 2 {3 4}}, {1,2,{3,4}})
%!assert ({1, 2 {3 4}}, {1,2,{3,4}})
%!assert ({1 2, {3 4}}, {1,2,{3,4}})
%!assert ({1 2 {3, 4}}, {1,2,{3,4}})
%!assert ({1, 2, {3 4}}, {1,2,{3,4}})
%!assert ({1 2,{3 4}}, {1,2,{3,4}})
%!assert ({1 2,{3,4}}, {1,2,{3,4}})
%!assert ({1,2,{3 4}}, {1,2,{3,4}})

## bug #43113 using null comma-separated list in constructor
%!test
%! z = cell (1,2,3,0,5);
%! assert ({1, z{:}, 2}, {1, 2});
%! assert ({1; z{:}; 2}, {1; 2});
%! assert ({1 2; z{:}; 3 4}, {1, 2; 3 4});
%! assert ({1 2; 5 z{:} 6; 3 4}, {1, 2; 5 6; 3 4});

## Tests for operator precedence as documented in section 8.8 of manual
## There are 13 levels of precedence from "parentheses and indexing" (highest)
## down to "statement operators" (lowest).
##
## Level 13 (parentheses and indexing)
## Overrides all other levels
%!test
%! a.b = 1;
%! assert (a. b++, 1);
%! assert (a.b, 2);
%! clear a;
%! a.b = [0 1];
%! b = 2;
%! assert (a.b', [0;1]);
%! assert (!a .b, logical ([1 0]));
%! assert (3*a .b, [0 3]);
%! assert (a. b-1, [-1 0]);
%! assert (a. b:3, 0:3);
%! assert (a. b>0.5, logical ([0 1]));
%! assert (a. b&0, logical ([0 0]));
%! assert (a. b|0, logical ([0 1]));
%! a.b = [1 2];
%! assert (a. b&&0, false);
%! assert (a. b||0, true);
%! a.b += a. b*2;
%! assert (a.b, [3 6]);
## Level 12 (postfix increment and decrement)
%!test
%! a = [3 5];
%! assert (2.^a ++, [8 32]);
%! assert (a, [4 6]);
%! assert (a--', [4; 6]);
%! assert (a, [3 5]);
%! a = 0;
%! assert (!a --, true);
%! assert (-a ++, 1);
%! assert (3*a ++, 0);
%! assert (a++-2, -1);
%! assert (1:a ++, 1:2);
%! assert (4>a++, true);
%! a = [0 -1];
%! assert ([1 1] & a++, logical ([0 1]));
%! assert ([0 0] | a++, logical ([1 0]));
%! a = 0;
%! assert (1 && a ++, false);
%! assert (0 || a --, true);
%! a = 5; b = 2;
%! b += a ++;
%! assert (b, 7);

## Level 11 (transpose and exponentiation)
%!test
%! a = 2;
%! assert (2 ^a++, 4);
%! assert (a, 3);
%! assert (2 ^--a ^2, 16);
%! assert (a, 2);
%! assert (2 ^++a, 8);
%! assert (a, 3);
%! assert (a' ^2, 9);
%! assert (2 ^sin (0), 1);
%! assert (-2 ^2, -4);
%! assert (2 ^+1 ^3, 8);
%! assert (2 ^-1 ^3, 0.125);
%! assert (2 ^~0 ^2, 4);
%! assert (!0 ^0, false);
%! assert (2*3 ^2, 18);
%! assert (2+3 ^2, 11);
%! assert ([1:10](1:2 ^2), [1 2 3 4]);
%! assert (3>2 ^2, false);
%! assert (1&0 ^0, true);
%! assert (0|0 ^0, true);
%! assert (1&&0 ^0, true);
%! assert (0||0 ^0, true);
%! a = 3;
%! a *= 0 ^0;
%! assert (a, 3);
## Level 10 (unary plus/minus, prefix increment/decrement, not)
%!test
%! a = 2;
%! assert (++ a*3, 9);
%! assert (-- a-2, 0);
%! assert (a, 2);
%! assert (! a-2, -2);
%! assert ([1:10](++ a:5), 3:5);
%! a = [1 0];
%! assert (! a>=[1 0], [false true]);
%! a = 0;
%! assert (++ a&1, true);
%! assert (-- a|0, false);
%! assert (-- a&&1, true);
%! assert (++ a||0, false);
%! a = 3;
%! a *= ++a;
%! assert (a, 16);
## Level 9 (multiply, divide)
%!test
%! assert (3+4 * 5, 23);
%! assert (5 * 1:6, [5 6]);
%! assert (3>1 * 5, false);
%! assert (1&1 * 0, false);
%! assert (1|1 * 0, true);
%! assert (1&&1 * 0, false);
%! assert (1||1 * 0, true);
%! a = 3;
%! a /= a * 2;
%! assert (a, 0.5);
## Level 8 (add, subtract)
%!test
%! assert ([2 + 1:6], 3:6);
%! assert (3>1 + 5, false);
%! assert (1&1 - 1, false);
%! assert (0|1 - 2, true);
%! assert (1&&1 - 1, false);
%! assert (0||1 - 2, true);
%! a = 3;
%! a *= 1 + 1;
%! assert (a, 6);
## Level 7 (colon)
%!test
%! assert (5:-1: 3>4, [true false false]);
%! assert (1: 3&1, [true true true]);
%! assert (1: 3|0, [true true true]);
%! assert (-1: 3&&1, false);
%! assert (-1: 3||0, false);
%! a = [1:3];
%! a += 3 : 5;
%! assert (a, [4 6 8]);
## Level 6 (relational)
%!test
%! assert (0 == -1&0, false);
%! assert (1 == -1|0, false);
%! assert (0 == -1&&0, false);
%! assert (1 == -1||0, false);
%! a = 2;
%! a *= 3 > 1;
%! assert (a, 2);
## Level 5 (element-wise and)
%!test
%! assert (0 & 1|1, true);
%! assert ([0 1] & 1&&1, false);
%! assert (0 & 1||1, true);
%! a = 2;
%! a *= 3 & 1;
%! assert (a, 2);
## Level 4 (element-wise or)
%!test
%! assert ([0 1] | 1&&0, false);
%! assert ([0 1] | 1||0, true);
%! a = 2;
%! a *= 0 | 1;
%! assert (a, 2);
## Level 3 (logical and)
%!test
%! assert (0 && 1||1, true);
%! a = 2;
%! a *= 3 && 1;
%! assert (a, 2);
## Level 2 (logical or)
%!test
%! a = 2;
%! a *= 0 || 1;
%! assert (a, 2);

## Tests for operator precedence within each level where ordering should
## be left to right except for postfix and assignment operators.

## Level 13 (parentheses and indexing)
%!test
%! a.b1 = 2;
%! assert (a.(strcat ('b','1'))++, 2);
%! assert (a.b1, 3);
%! b = {1 2 3 4 5};
%! assert (b{(a. b1 + 1)}, 4);
%! b = 1:5;
%! assert (b(a. b1 + 1), 4);
%! assert ([2 3].^2', [4; 9]);
## Level 12 (postfix increment and decrement)
## No tests possible since a++-- is not valid
## Level 11 (transpose and exponentiation)
%!test
%! assert (2^3^2, 64);
%! assert ([2 3].^2.', [4;9]);
%! assert ([2 3].'.^2, [4;9]);
%! assert (3*4i'.', 0 - 12i);
%! assert (3*4i.'.', 0 + 12i);
%! assert (2^-4^3, (1/16)^3);
%! assert (2^+4^3, 16^3);
%! assert (2^~0^2, 4);

## Level 10 (unary plus/minus, prefix increment/decrement, not)
%!test
%! assert (+-+1, -1);
%! a = -1;
%! assert (!++a, true);
%! assert (a, 0);
%! assert (-~a, -1);
%! assert (!~--a, true);
%! assert (a, -1);
## Level 9 (multiply, divide)
%!test
%! assert (3 * 4 / 5, 2.4);
%! assert (3 ./ 4 .* 5, 3.75);
%! assert (2 * 4 \ 6, 0.75);
%! assert (2 .\ 4 .* 6, 12);
## Level 8 (add, subtract)
%!test
%! assert (-3 - 4 + 1 + 3 * 2, 0);
## Level 7 (colon)
## No tests possible because colon operator can't be combined
## with second colon operator.
## Level 6 (relational)
%!test
%! assert (0 < 1 <= 0.5 == 0 >= 0.5 > 0, true);
%! assert (1 < 1 == 0 != 0, true);
%! assert (1 < 1 == 0 ~= 0, true);
## Level 5 (element-wise and)
## No tests possible.  Only one operator (&) at this precedence level
## and operation is associative.
## Level 4 (element-wise or)
## No tests possible.  Only one operator (|) at this precedence level
## and operation is associative.
## Level 3 (logical and)
%!test
%! a = 1;
%! assert (1 && 0 && ++a, false);
%! assert (a, 1);
## Level 2 (logical or)
%!test
%! a = 1;
%! assert (0 || 1 || ++a, true);
%! assert (a, 1);
## Level 1 (assignment)
%!test
%! a = 2; b = 5; c = 7;
%! assert (a += b *= c += 1, 42);
%! assert (b == 40 && c == 8);

## Test extended number format which allows '_' as NOP character
%!assert (123_456, 123456)
%!assert (.123_456, .123456)
%!assert (123_456.123_456, 123456.123456)
%!assert (0xAB_CD, uint16 (43981))
%!assert (2e0_1, 20)

## Test binary constants
%!assert (0b101, uint8 (5))
%!assert (0B1100_0001, 0xC1)
%!assert (class (0b1), "uint8")

## Test range of large binary and hexadecimal literals
%!assert (0x8000_0000_0000_0000, uint64 (2^63))
%!assert (0xFFFF_FFFF_FFFF_FFFF, uint64 (2^64))
%!assert (0b10000000_0000000_000000000_00000000_00000000_00000000_00000000_00000000, uint64 (2^63))
%!assert (0b11111111_1111111_111111111_11111111_11111111_11111111_11111111_11111111, uint64 (2^64))

## Test creation of anonymous functions

%!test
%! af_in_cell = {@(x) [1 2]};
%! assert (af_in_cell{1}(), [1, 2]);

%!test
%! R = @(rot) [cos(rot) -sin(rot); sin(rot) cos(rot)];
%! assert (R(pi/2), [cos(pi/2), -sin(pi/2); sin(pi/2),cos(pi/2)]);

## Check that xyz is tagged as a variable in the parser.  Both
## expressions must remain on one line for this test to work as
## intended.
%!test
%! xyz(1) = 1; xyz /= 1;
%! assert (xyz, 1);

%!test
%! warning ("off", "Octave:num-to-str", "local");
%! a = [97 ... % comment
%!      'b'];
%! assert (a, 'ab');

## Check that a cell array containing function handles is parsed
## correctly with or without commas.
%!test
%! a = {1, @sin, 2, @cos};
%! b = {1 @sin 2 @cos};
%! assert (a, b);

## Maybe unnecessary, but check that further changes to parser don't
## invalidate error handling (bug #46534).
#!error <vertical dimensions mismatch \(1x2 vs 1x1\)> z = [1, 2; 3]

%!test
%! f = @(s,t=toeplitz (s),u=t(x=2:end-1,x)=32)t;
%! assert (f (1), 1);
%! assert (f (1, 2), 2);

## FIXME: We need a sequence of concatenation tests since this seems
##        to be a frequently reported source of incompatibilities w/Matlab
## Check concatenation of empty char matrices (bug #52542)
%!assert (double ([char(ones(0,3)); 'A']), 65)
%!assert (double ([char(ones(0,3)); char(ones(2,0)); 'A']), 65)

%!test <*54465>
%! x = 0;
%! if (! 0 || (x = 1))
%! endif
%! assert (x == 0);

%!function x = matlab_short_circuit ()
%!  x = 0;
%!  if (! 0 | (x = 1))
%!  endif
%!endfunction
%!test <*54465>
%! fail ("x = matlab_short_circuit ()", "warning", "Matlab-style short-circuit operation performed for operator |");
%! assert (x == 0);

%!test <*56959>
%! fail ("i:5", "warning", "imaginary part of complex colon arguments is ignored");
%! fail ("1:5*i", "warning", "imaginary part of complex colon arguments is ignored");
%! fail ("1:i:5", "warning", "imaginary part of complex colon arguments is ignored");

