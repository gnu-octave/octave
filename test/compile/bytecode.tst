########################################################################
##
## Copyright (C) 2022-2024 The Octave Project Developers
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
## <https://www.gnu.org/Test/>.
##
########################################################################

## Just clear the cached string in __prog_output_assert__() and clear
## classes method lookups due to maybe bug
%!test
%! __prog_output_assert__ ("");
%! % Overloading of class-methods seems to stick so we need to clear them since we overload
%! % double's display. Is this a bug ???
%! clear classes

## Test binary expressions
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local"); % Disable the vm for the tree_walker run
%!
%! clear all % We want all compiled functions to be cleared so that we can run the tree_walker
%!
%! key = "10 -10 24 0.041666666666666664 1 -5.0915810909090906 13 1 0 1 0 truthy1 1 falsy3 falsy4 truthy5 1 truthy7 truthy8 1 falsy9 falsy11 0 1 0 1 0 0 1 1 0 1 0 1 1 1 0 1 0 1 0 0 0 0 1 1 1 1 1 1 1 1 ";
%!
%! __vm_compile__ bytecode_binops clear;
%! bytecode_binops ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! % We wanna know the function compiles, so do a explicit compile
%! assert (__vm_compile__ ("bytecode_binops"));
%! bytecode_binops ();
%! assert (__prog_output_assert__ (key));

## Test subfunctions
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "2 2 2 11 30 10 30 5  0 0 double 1 2 1 2 double 30 11 5  0 0 double 1 2 1 2 double 11 11 12 13 1 1 double 14 1 1 double 11 11 5 13 1 1 double 14 1 1 double 11 3 3 3 2 2 2 313 ret32:1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 ret32:1 ret32:ret32:1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 take32:1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 1 18 59 64 ";
%! a = 313;
%! % Gets called to ensure anonymous functions calls with
%! % externally scoped variables work
%! h = @() __printf_assert__ ("%d ", a);
%!
%! __vm_compile__ bytecode_subfuncs clear;
%! bytecode_subfuncs (h);
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_subfuncs"));
%! bytecode_subfuncs (h);
%! assert (__prog_output_assert__ (key));

## Test if:s
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "0 1 2 3 4 5 6 7 8 1 2 yay1 3 5 7 8 1 yay1 3 4 yay2 5 6 7 yay3 ";
%!
%! __vm_compile__ bytecode_if clear;
%! bytecode_if ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_if"));
%! bytecode_if ();
%! assert (__prog_output_assert__ (key));

## Test for:s
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "1 2 3 4 4 1 3 5 5 1 4 4 4 3 2 1 1 0.200 0.300 0.400 0.400 0.300 0.200 0.100 0.000 0.000 NaN NaN NaN 1 4 2 2 16 4 3 3 256 3 2 1  double 1 3 size 2 size 1 2 4 size 2 size 1  double q size 1 size 1 w size 1 size 1 e size 1 size 1 char single single 5 1 11 2 12 key:a val:1 1val:1 key:b val:1 3val:2 4val:2 2key:c val:string ";
%!
%! __vm_compile__ bytecode_for clear;
%! bytecode_for ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_for"));
%! bytecode_for ();
%! assert (__prog_output_assert__ (key));

## Test while
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "5 4 3 2 1 3 5 4 4 3 3 4 1 2 1 3 2 8 3 1 3 ";
%!
%! __vm_compile__ bytecode_while clear;
%! bytecode_while ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_while"));
%! bytecode_while ();
%! assert (__prog_output_assert__ (key));

## Test assign
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "2 3 1 1 2 3 2 3 2 2 6 18 2.000000 2.000000 3.000000 4.000000 5.000000 1 4 double 729.000000 324.000000 182.250000 116.640000 4 1 double 37.000000 81.000000 54.000000 118.000000 2 2 double ";
%!
%! __vm_compile__ bytecode_assign clear;
%! bytecode_assign ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_assign"));
%! bytecode_assign ();
%! assert (__prog_output_assert__ (key));

## Test unary
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "-1 4 1 2 3 4 1 3 2 4 0 0 ";
%!
%! __vm_compile__ bytecode_unary clear;
%! bytecode_unary ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_unary"));
%! bytecode_unary ();
%! assert (__prog_output_assert__ (key));

## Test range
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "1 2 3 1 3 5 1 3 5 1 1.1 1.2 1.3 1.4 1 0.9 0.8 0.7 7 7 1 8 10 8 10 8 9 10 11 8 9 10 11 10 8 10 8 -10 -9 -8 -7 -10 -9 -8 -7 ";
%!
%! __vm_compile__ bytecode_range clear;
%! bytecode_range ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_range"));
%! bytecode_range ();
%! assert (__prog_output_assert__ (key));

## Test multi assign
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "3 4 2 2 1 2 3 4 1 2 3 4 1 1 3 2 3 4 1 1 1 2 3 4 1 1 1 2 3 4 1 2 3 ";
%!
%! __vm_compile__ bytecode_multi_assign clear;
%! bytecode_multi_assign ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_multi_assign"));
%! bytecode_multi_assign ();
%! assert (__prog_output_assert__ (key));

## Test subsasgn
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! clear functions  % persistent variables in bytecode_subsasgn
%! key = "3 5 9 8 11 13 1 2 3 4 5 6 77 88 99 1010 1 2 3 987 987 6 77 88 99 1010 0 0 0 0 0 13 double 3 2 4 2 3 cell 1 3 6 7 2 3 1 4 5 1 3 5 2 4 6 7 7 7 7 7 7 1 2 3 1 3 3 2 3 2 3 1 3 1 2 3 4 4 4 3 4 5 6 1 5 3 4 1 5 -1 4 1 5 -1 8 3 3 3 3 3 3 3 3 1 1 3 1 ";
%!
%! __vm_compile__ bytecode_subsasgn clear;
%! bytecode_subsasgn ();
%! assert (__prog_output_assert__ (key), "bytecode_subsasgn failed uncompiled");
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_subsasgn"));
%! bytecode_subsasgn ();
%! assert (__prog_output_assert__ (key), "bytecode_subsasgn failed compiled");

## Test end
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "1 3 2 4 1 5 6 7 2 2 5 5 6 6 1 2 3 4 5 2 2 2 3 3 4 fs 2 3 1 foo oo ";
%!
%! __vm_compile__ bytecode_end clear;
%! bytecode_end ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_end"));
%! bytecode_end ();
%! assert (__prog_output_assert__ (key));

## Test matrix
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "1 2 3 4 1 4 1 2 3 4 4 1 1 3 2 4 2 2 1 3 1 3 2 4 2 4 4 2  0 0 a b c d 7 15 10 22 2 2 30 1 1 1 2 3 4 2 4 6 8 3 6 9 12 4 8 12 16 4 4 1 1 1 0.0333333 0.0666667 0.1 0.133333 0.0666667 0.133333 0.2 0.266667 0.1 0.2 0.3 0.4 0.133333 0.266667 0.4 0.533333 4 4 1 0 0 1 2 2 30 1 1 10 14 14 20 2 2 0.0333333 0.0666667 0.1 0.133333 0.0666667 0.133333 0.2 0.266667 0.1 0.2 0.3 0.4 0.133333 0.266667 0.4 0.533333 4 4 2.5 -0.5 2 0 2 2 2 6 4 8 2 2 2 3 4 5 3 4 5 6 4 5 6 7 5 6 7 8 4 4 3 4 5 6 1 4 3 4 5 6 1 4 -1 0 1 2 1 4 2 4 6 8 1 4 0.5 1 1.5 2 1 4 0.5 1 1.5 2 1 4 1 4 9 16 1 4 1 1 1 1 1 4 1 1 1 1 1 4 1 4 27 256 1 4 1 2 3 4 2 4 6 8 3 6 9 12 4 8 12 16 4 4 1 0.5 0.333333 0.25 2 1 0.666667 0.5 3 1.5 1 0.75 4 2 1.33333 1 4 4 1 2 3 4 0.5 1 1.5 2 0.333333 0.666667 1 1.33333 0.25 0.5 0.75 1 4 4 1 4 27 256 4 1 qzwxeca s d  zzxxccz x c  1 258 33264 258 1 33264 ";
%!
%! __vm_compile__ bytecode_matrix clear;
%! bytecode_matrix ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_matrix"));
%! bytecode_matrix ();
%! assert (__prog_output_assert__ (key));

## Test return
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "2 baaar bääär baaaaz bääääz bååååz booz 1 1 2 1 1 1 2 1 silly silly ";
%!
%! __vm_compile__ bytecode_return clear;
%! bytecode_return ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_return"));
%! bytecode_return ();
%! assert (__prog_output_assert__ (key));

## Test word list command
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "A B C QWE ";
%!
%! __vm_compile__ bytecode_wordlistcmd clear;
%! bytecode_wordlistcmd ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_wordlistcmd"));
%! bytecode_wordlistcmd ();
%! assert (__prog_output_assert__ (key));

## Test do until
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "5 3 5 5 4 4 3 4 1 2 1 3 2 12 3 0 3 ";
%!
%! __vm_compile__ bytecode_dountil clear;
%! bytecode_dountil ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_dountil"));
%! bytecode_dountil ();
%! assert (__prog_output_assert__ (key));

## Test cell
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "a b a b 1 2 b c b c 1 2 char b c c d b d c e 2 2 b d f h j l c e g i k m 6 2 1 2 2 3 1 3 2 4 1 3 1 2 1 3 2 4 2 2 double qwe 1 3 char 1 2 ";
%!
%! __vm_compile__ bytecode_cell clear;
%! bytecode_cell ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_cell"));
%! bytecode_cell ();
%! assert (__prog_output_assert__ (key));

## Test varargin
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "2 3 1 2 1 1 1 1 1 2 3 4 1 4 4 0 0 0 1 2 3 4 1 3 4 2 3 4 1 3 3 2 2 3 4 1 4 4 1 0 0 1 0 0 0 2 1 1 1 1 2 3 4 1 3 4 2 3 4 1 3 3 2 2 3 4 1 4 4 1 2 1 2 4 1 2 0 0 2 1 nob 0 0 1 noa nob 0 0 0 2 1 2 4 1 2 3 4 3 3 2 1 0 ";
%!
%! __vm_compile__ bytecode_varargin clear;
%! bytecode_varargin (1,2,3);
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_varargin"));
%! bytecode_varargin (1,2,3);
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (0, "local");
%! clear all
%! key = "0 0 1 1 1 1 1 2 3 4 1 4 4 0 0 0 1 2 3 4 1 3 4 2 3 4 1 3 3 2 2 3 4 1 4 4 1 0 0 1 0 0 0 2 1 1 1 1 2 3 4 1 3 4 2 3 4 1 3 3 2 2 3 4 1 4 4 1 2 1 2 4 1 2 0 0 2 1 nob 0 0 1 noa nob 0 0 0 2 1 2 4 1 2 3 4 1 3 2 1 0 ";
%!
%! __vm_compile__ bytecode_varargin clear;
%! bytecode_varargin (1);
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_varargin"));
%! bytecode_varargin (1);
%! assert (__prog_output_assert__ (key));

## Test global variables
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "double 0 0 1 1 1 1 1 2 2 2 400 100 0 1 3 double 1 1 1 2 double 1 2 1 1 11 eclass:double 1 1 3 4 double 1 2 400 100 1 1 1 1 3 4 1 1 5 6 1 1 1 2 double 1 2 1 2 double 1 2 1 1 3 4 eclass:double 1 2 3 4 double 1 2 0 0 1 1 3 4 1 1 5 6 1 0 2 double 2 double 11 2 6 4 5 double 1 5 11 double 1 1 22 double 1 1 33 double 1 1 3 double 1 1 4 double 1 1 10 double 1 1 2 3 double 1 2 3 double 1 1 2 double 1 1 55 double 1 1 7 double 1 1 0 11 12 4 11 12 4 ";
%!
%! __vm_compile__ bytecode_global_1 clear;
%! clear global a;
%! clear global b;
%! clear global q
%! global q % Used in test function
%! q = 55;
%! bytecode_global_1 ();
%! assert (__prog_output_assert__ (key));
%! assert (length(who('global','a')));
%! assert (length(who('global','b')));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_global_1"));
%! clear global a;
%! clear global b;
%! clear global q;
%! global q % Used in test function
%! q = 55;
%! bytecode_global_1 ();
%! assert (length(who('global','a')));
%! assert (length(who('global','b')));
%! assert (__prog_output_assert__ (key));
%!
%! global a b;
%! assert (a == 5);
%! assert (b == 6);
%!
%! clear global a;
%! clear global b;
%! clear global q;

## Test switch
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "yay yay2 yay3 yay4 yay5 yay6 yay7 yay8 1 2 3 3 1 3 3 4 4 1 3 3 4 4 2 yoo 2 3 3 1:1 for-end:12:2 3:3 for-end:3breaking:4 ";
%!
%! __vm_compile__ bytecode_switch clear;
%! bytecode_switch;
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_switch"));
%! bytecode_switch;
%! assert (__prog_output_assert__ (key));

## Test eval (dynamic stack frames)
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "3.000000 1.000000 1.000000 double 4.000000 1.000000 1.000000 double 4.000000 1.000000 1.000000 double 5.000000 4.000000 2.000000 3.000000 1.000000 1.000000 double 4.000000 1.000000 1.000000 double 4.000000 1.000000 1.000000 double 5.000000 4.000000 2.000000 1:11.000000 2:22.000000 3:33.000000 4:3.000000 5:22.000000 6:3.000000 7:3.000000 3 3 2 2 3.000000 3.000000 ";
%!
%! __vm_compile__ bytecode_eval_1 clear;
%! bytecode_eval_1;
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_eval_1"));
%! bytecode_eval_1;
%! assert (__prog_output_assert__ (key));

## Test evalin and assignin
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! % We want to test all combinations of compiled and uncompiled evalin_1 and 2.
%!
%! key = "2.000000 yoyo yobase 3.000000 yoyo2 yobase2 123.000000 124.000000 11.000000 33.000000 ";
%!
%! caller_a = 2;
%!
%!
%! __vm_compile__ bytecode_evalin_1 clear;
%! __vm_compile__ bytecode_evalin_2 clear;
%! bytecode_evalin_1 ();
%! assert (__prog_output_assert__ (key));
%!
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_evalin_1"));
%! bytecode_evalin_1 ();
%! assert (__prog_output_assert__ (key));
%!
%!
%! __vm_compile__ bytecode_evalin_1 clear;
%! __vm_compile__ bytecode_evalin_2 clear;
%! assert (__vm_compile__ ("bytecode_evalin_1"));
%! assert (__vm_compile__ ("bytecode_evalin_2"));
%! bytecode_evalin_1 ();
%! assert (__prog_output_assert__ (key));
%!
%! __vm_compile__ bytecode_evalin_1 clear;
%! __vm_compile__ bytecode_evalin_2 clear;
%! assert (__vm_compile__ ("bytecode_evalin_2"));
%! bytecode_evalin_1 ();
%! assert (__prog_output_assert__ (key));
%!

## Test error messages
%!testif ENABLE_BYTECODE_EVALUATOR
%! ## Interpreter reference
%! __vm_enable__ (0, "local");
%! clear all
%! __vm_compile__ bytecode_errors clear;
%! fail ("bytecode_errors (0)", ...
%!       "'qweqwe' undefined near line 9, column 6");
%! fail ("bytecode_errors (1)", ...
%!       "'b' undefined near line 15, column 7");
%! fail ("bytecode_errors (2)", ...
%!       "'b' undefined near line 19, column 7");
%! fail ("bytecode_errors (3)", ...
%!       "'b' undefined near line 23, column 7");
%! fail ("bytecode_errors (4)", ...
%!       "'b' undefined near line 27, column 3");
%! fail ("bytecode_errors (5)", ...
%!       "'b' undefined near line 31, column 3");
%! fail ("bytecode_errors (6)", ...
%!       'a\(3\): out of bound 2 \(dimensions are 1x2\)');
%! fail ("bytecode_errors (7)", ...
%!       'a\(-1\): subscripts must be either integers 1 to \(2\^(31|63)\)-1 or logicals');
%! fail ("bytecode_errors (8)", ...
%!       'operator \+: nonconformant arguments \(op1 is 1x3, op2 is 1x2\)');
%!
%! __vm_enable__ (1, "local");
%! ## Bytecode running the same errors
%! __vm_compile__ bytecode_errors;
%! fail ("bytecode_errors (0)", ...
%!       "'qweqwe' undefined near line 9, column 6");
%! fail ("bytecode_errors (1)", ...
%!       "'b' undefined near line 15, column 7");
%! fail ("bytecode_errors (2)", ...
%!       "'b' undefined near line 19, column 7");
%! fail ("bytecode_errors (3)", ...
%!       "'b' undefined near line 23, column 7");
%! fail ("bytecode_errors (4)", ...
%!       "'b' undefined near line 27, column 3");
%! fail ("bytecode_errors (5)", ...
%!       "'b' undefined near line 31, column 3");
%! fail ("bytecode_errors (6)", ...
%!       'a\(3\): out of bound 2 \(dimensions are 1x2\)');
%! fail ("bytecode_errors (7)", ...
%!       'a\(-1\): subscripts must be either integers 1 to \(2\^(31|63)\)-1 or logicals');
%! fail ("bytecode_errors (8)", ...
%!       'operator \+: nonconformant arguments \(op1 is 1x3, op2 is 1x2\)');

## Test try catch
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "yay yay2 yay3 ooo yay2 yay3 ooo2 ooo2 yay3 yay4 Nested error yay5 yay6 In catch yay7 qwe yay8 Error in subfunction yay9 'asd' undefined near line 87, column 11 yay10 operator *: nonconformant arguments (op1 is 1x2, op2 is 1x3) yay11 yoyo yay12 foo yay12 foo yay12 foo yay13 foo yay13 foo yay13 foo ";
%!
%! __vm_compile__ bytecode_trycatch clear;
%! bytecode_trycatch;
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_trycatch"));
%! bytecode_trycatch;
%! assert (__prog_output_assert__ (key));

## Test unwind protect
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "yay1 yay2 yay3 e1 subyyay1 subyyay2 subyyay3 subyyay4 subyyay5 subyyay6 subyyay7 subyyay8 subyyay9 subyyay10 subyyay11 subyyay12 subyyay13 subyyay14 subyyay15 subyyay16 subyyay17 subyyay18 yay4 yay5 yay6 ";
%! __vm_compile__ bytecode_unwind clear;
%! bytecode_unwind;
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_unwind"));
%! bytecode_unwind;
%! assert (__prog_output_assert__ (key));

## Test persistant
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! clear functions  % clear persistent variables in bytecode_persistant
%! key = "a:3 b: double 0 0 0 c:3 c:4 a:4 b:1 double 1 1 0 c:5 c:6 ";
%!
%! __vm_compile__ bytecode_persistant clear;
%! bytecode_persistant;
%! bytecode_persistant;
%! assert (__prog_output_assert__ (key));
%!
%! clear all;
%! __vm_enable__ (1, "local");
%! key = "a:3 b: double 0 0 0 c:3 c:4 a:4 b:1 double 1 1 0 c:5 c:6 ";
%! assert (__vm_compile__ ("bytecode_persistant"));
%! bytecode_persistant;
%!
%! bytecode_persistant;
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (0, "local");
%! __vm_compile__ bytecode_persistant clear;
%! clear all;
%! key = "a:3 b: double 0 0 0 c:3 c:4 a:4 b:1 double 1 1 0 c:5 c:6 ";
%! bytecode_persistant;
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_persistant"));
%!
%! bytecode_persistant;
%! assert (__prog_output_assert__ (key));

## Test structs
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "1 2 double 1 1 struct 3 4 ";
%! __vm_compile__ bytecode_struct clear;
%! bytecode_struct;
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_struct"));
%! bytecode_struct;
%! assert (__prog_output_assert__ (key));

## Test indexing chained objects and strange indexing
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "2 2 3 3 2 cell 1 1 3 3 2 3 22 double 33 3 4 matlab.lang.MemoizedFunction 2 ";
%! __vm_compile__ bytecode_index_obj clear;
%! bytecode_index_obj;
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_index_obj"));
%! bytecode_index_obj;
%! assert (__prog_output_assert__ (key));

## Test varargout
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "7 8 1 1 2 1 0 0 0 1 0 1 0 0 0 1 0 ";
%! __vm_compile__ bytecode_varargout clear;
%! bytecode_varargout;
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_varargout"));
%! bytecode_varargout;
%! assert (__prog_output_assert__ (key));

## Test inputname
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "a a b + 1  a a b b aa aa bb bb aa + 1  bb * 3  a + 1  b * 3  aa aa bb bb aa + 1  bb * 3  a a b b a + 1  b * 3  ";
%! __vm_compile__ bytecode_inputname clear;
%! a = 9; b = 8;
%! bytecode_inputname (a, b + 1);
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_inputname"));
%! bytecode_inputname (a, b + 1);
%! assert (__prog_output_assert__ (key));

## Test ans
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "2 5 1 1 1 ";
%! __vm_compile__ bytecode_ans clear;
%! bytecode_ans;
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_ans"));
%! bytecode_ans;
%! assert (__prog_output_assert__ (key));

## Test using classdef
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! global cdef_foo_ctor_cnt; clear global cdef_foo_ctor_cnt;
%! global cdef_foo_dtor_cnt; clear global cdef_foo_dtor_cnt;
%! key = ". 1 f1 . 2 f3 3 f2 . sumf2f3 2 . . call14 f4 . a a_1 . 5 f8 . 6 f10 7 f9 . sumf9f10 2 . . call18 f11 . 2 2 3 4 4 4 3 2 2 3 . 9 sumf9f10 10 f12 11 f13 12 f14 13 sumf2f3 14 f5 15 f6 16 f7 ";
%! __vm_compile__ bytecode_cdef_use clear;
%! bytecode_cdef_use ();
%! assert (__prog_output_assert__ (key));
%! global cdef_foo_ctor_cnt; global cdef_foo_dtor_cnt;
%! assert (cdef_foo_ctor_cnt == cdef_foo_dtor_cnt); % Check, as many ctor and dtor executions
%!
%! __vm_enable__ (1, "local");
%! global cdef_foo_ctor_cnt; clear global cdef_foo_ctor_cnt;
%! global cdef_foo_dtor_cnt; clear global cdef_foo_dtor_cnt;
%! assert (__vm_compile__ ("bytecode_cdef_use"));
%! bytecode_cdef_use ();
%! assert (__prog_output_assert__ (key));
%! global cdef_foo_ctor_cnt; global cdef_foo_dtor_cnt;
%! assert (cdef_foo_ctor_cnt == cdef_foo_dtor_cnt);
%!
%! global cdef_foo_ctor_cnt; clear global cdef_foo_ctor_cnt;
%! global cdef_foo_dtor_cnt; clear global cdef_foo_dtor_cnt;
%!
%! clear global __assert_printf__

## Test anonymous function handles
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "1 2 12 3 4 4 4 1 2 3 1 2 3 1 2 1 2 11 12 11 12 1 4 4 1 1 1 2 1 2 1 2 3 1 2 3 1 3 1 3 9 0 1 fooo ~101 103 ~101 103 ~110 123 ~101 123 ~010 123 ~000 123 ~011 123 ";
%! __vm_compile__ bytecode_anon_handles clear;
%! bytecode_anon_handles;
%!
%! global __assert_printf__;
%!
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_anon_handles"));
%! bytecode_anon_handles;
%! global __assert_printf__;
%! assert (__prog_output_assert__ (key));

## Test compling a function named differently from its
## m-file
%!testif ENABLE_BYTECODE_EVALUATOR
%! clear all
%! __vm_enable__ (1, "local");
%! __vm_compile__ wrongname_fn clear;
%! assert (__vm_compile__ ("wrongname_fn"));
%!
%! assert (wrongname_fn (77) == 78);

## Test some misc stuff
%!testif ENABLE_BYTECODE_EVALUATOR
%! clear all
%! __vm_enable__ (1, "local");
%!
%! bytecode_misc; % asserts inernally

## Leak check
%!testif ENABLE_BYTECODE_EVALUATOR
%! clear all
%! __vm_enable__ (1, "local");
%!
%! c = 2;
%! d = 3;
%! n_c = __ref_count__ (c);
%! n_d = __ref_count__ (d);
%! bytecode_leaks (c, d); % asserts inernally
%!
%! assert (n_c == __ref_count__ (c))
%! assert (n_d == __ref_count__ (d))

## Test scripts
%!testif ENABLE_BYTECODE_EVALUATOR
%! __vm_enable__ (0, "local");
%! clear all
%! key = "0 1 3 4 5 3 ";
%! __vm_compile__ bytecode_scripts clear;
%! bytecode_scripts;
%! assert (__prog_output_assert__ (key));
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_scripts"));
%! bytecode_scripts;
%! assert (__prog_output_assert__ (key));

## Test nested functions.
%!testif ENABLE_BYTECODE_EVALUATOR
%! global cdef_bar_cnt
%! cdef_bar_cnt = 0;
%!
%! __vm_enable__ (0, "local");
%! clear all
%! __vm_compile__ bytecode_nested clear;
%! % These tests uses asserts in themself
%! bytecode_nested;
%!
%! cdef_bar_cnt = 0;
%!
%! __vm_enable__ (1, "local");
%! assert (__vm_compile__ ("bytecode_nested"));
%! bytecode_nested;
%!
%! assert (cdef_bar_cnt == 0);
%! clear -global cdef_bar_alive_objs cdef_bar_cnt glb_d glb_e glb_f

## Test script interaction when called from top scope
%!testif ENABLE_BYTECODE_EVALUATOR
%!
%! __vm_enable__ (0, "local");
%! clear all
%!
%! global bytecode_script_topscope_place; % Used in bytecode_script_topscope the select where to evalin
%! bytecode_script_topscope_place = "base";
%!
%! global bytecode_script_topscope_call_self
%! bytecode_script_topscope_call_self = false;
%!
%! bytecode_script_topscope_setup; % Does some setups of globals and locals in base frame, from a function
%! evalin ("base", "bytecode_script_topscope_setup_script"); % Also does some setups, but from a script
%! evalin ("base", "bytecode_script_topscope"); % A script that does things a does some asserts
%!
%! bytecode_script_topscope_assert; % Does some asserts and cleans up the globals and locals added
%!
%! __vm_enable__ (1, "local");
%! bytecode_script_topscope_setup;
%! evalin ("base", "bytecode_script_topscope_setup_script");
%! assert (__vm_compile__ ("bytecode_script_topscope"));
%! evalin ("base", "bytecode_script_topscope");
%! bytecode_script_topscope_assert;
%!
%! %% Redo the test, but nested in itself to get a stack frame in between.
%! bytecode_script_topscope_call_self = true;
%!
%! bytecode_script_topscope_setup;
%! evalin ("base", "bytecode_script_topscope_setup_script");
%! assert (__vm_compile__ ("bytecode_script_topscope"));
%! evalin ("base", "bytecode_script_topscope");
%! bytecode_script_topscope_assert;
%!
%! bytecode_script_topscope_call_self = false;
%!
%! %% Redo the test, but in a command line function instead of in the top scope.
%! bytecode_script_topscope_place = "caller"; % Governs some evalin() in nbytecode_script_topscope. Switch to "caller"
%!
%! eval ("function bytecode_script_topscope_cli_fn ()\nbytecode_script_topscope_setup ('caller');\nbytecode_script_topscope_setup_script;\nbytecode_script_topscope;\nbytecode_script_topscope_assert ('caller');\nend");
%!
%! bytecode_script_topscope_cli_fn ();
%! bytecode_script_topscope_call_self = true;
%! bytecode_script_topscope_cli_fn ();
%!
%! %% Redo the test, but test all 15 different combinations of compiled and uncompiled functions and scripts
%!
%! __vm_enable__ (0, "local");
%! clear all
%!
%! global bytecode_script_topscope_place % An assert in nbytecode_script_topscope need to know whether to check in caller or base
%! global bytecode_script_topscope_call_self; % Whether nbytecode_script_topscope should call it self to add another frame
%!
%! names = {"bytecode_script_topscope_cli_fn", "bytecode_script_topscope_setup", "bytecode_script_topscope", "bytecode_script_topscope_assert", "bytecode_script_topscope_setup_script"};
%!
%! for k = 0:5
%!   choices = nchoosek (names, k);
%!   for choise = choices'
%!     clear bytecode_script_topscope_cli_fn bytecode_script_topscope_setup nbytecode_script_topscope nbytecode_script_topscope_assert bytecode_script_topscope_setup_script
%!
%!     eval ("function bytecode_script_topscope_cli_fn ()\nbytecode_script_topscope_setup ('caller');\nbytecode_script_topscope_setup_script;\nbytecode_script_topscope;\nbytecode_script_topscope_assert ('caller');\nend");
%!
%!     for fn = choise'
%!       assert (__vm_compile__ (fn{1}))
%!     end
%!
%!     % Check if it works in the CLI function
%!     bytecode_script_topscope_place = "caller";
%!     bytecode_script_topscope_call_self = false;
%!     bytecode_script_topscope_cli_fn ();
%!     bytecode_script_topscope_call_self = true;
%!     bytecode_script_topscope_cli_fn ();
%!
%!     % Check if it works in base
%!     bytecode_script_topscope_place = "base";
%!     bytecode_script_topscope_call_self = false;
%!     evalin ("base", "bytecode_script_topscope_setup ('base');\nbytecode_script_topscope_setup_script;\nbytecode_script_topscope;\nbytecode_script_topscope_assert ('base');")
%!     bytecode_script_topscope_call_self = true;
%!     evalin ("base", "bytecode_script_topscope_setup ('base');\nbytecode_script_topscope_setup_script;\nbytecode_script_topscope;\nbytecode_script_topscope_assert ('base');")
%!   end
%! end
%!
%! %% Cleanup after the tests
%! clear global bytecode_script_topscope_call_self
%! clear global bytecode_script_topscope_place
%! clear bytecode_script_topscope_cli_fn

## Test save() and load() from scripts.
%!testif ENABLE_BYTECODE_EVALUATOR
%! clear all
%!
%! for use_vm = [true, false] % Test with and without VM
%!   __vm_enable__ (use_vm, "local");
%!
%!   % We need to pass the file name to the scripts with a global
%!   global bytecode_load_script_file
%!   bytecode_load_script_file = [tempname(), ".mat"];
%!
%!   eval ("function bytecode_load_script_save_fn\n  bytecode_load_script_save\n;  bytecode_load_script_load_and_assert;\n  end\n")
%!
%!   if use_vm
%!     assert (__vm_compile__ ("bytecode_load_script_save"))
%!     assert (__vm_compile__ ("bytecode_load_script_load_and_assert"))
%!     assert (__vm_compile__ ("bytecode_load_script_save_fn"))
%!   end
%!
%!   unwind_protect
%!     evalin ("base", "bytecode_load_script_save"); % Saves some variables to a file with save ()
%!     evalin ("base", "bytecode_load_script_load_and_assert"); % Loads them back and checks their values etc
%!   unwind_protect_cleanup
%!     unlink (bytecode_load_script_file);
%!     evalin ("base", "clear local_aa local_bb local_cc")
%!     evalin ("base", "clear global glb_aa glb_bb glb_cc glb_dd glb_ee")
%!   end_unwind_protect
%!
%!   bytecode_load_script_file = [tempname(), ".mat"];
%!   %% Same test, but from a function
%!   unwind_protect
%!     bytecode_load_script_save_fn;
%!   unwind_protect_cleanup
%!     unlink (bytecode_load_script_file);
%!   end_unwind_protect
%!
%!   clear all
%! end
%!
