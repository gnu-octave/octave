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

## -*- texinfo -*-
## @table @code
## @item Octave:array-to-scalar
## If the @code{Octave:array-to-scalar} warning is enabled, Octave will
## warn when an implicit conversion from an array to a scalar value is
## attempted.  By default, the @code{Octave:array-to-scalar} warning is
## disabled.
##
## @item Octave:array-to-vector
## If the @code{Octave:array-to-vector} warning is enabled, Octave will
## warn when an implicit conversion from an array to a vector value is
## attempted.  By default, the @code{Octave:array-to-vector} warning is
## disabled.
##
## @item Octave:assign-as-truth-value
## If the @code{Octave:assign-as-truth-value} warning is
## enabled, a warning is issued for statements like
##
## @example
## @group
## if (s = t)
##   @dots{}
## @end group
## @end example
##
## @noindent
## since such statements are not common, and it is likely that the intent
## was to write
##
## @example
## @group
## if (s == t)
##   @dots{}
## @end group
## @end example
##
## @noindent
## instead.
##
## There are times when it is useful to write code that contains
## assignments within the condition of a @code{while} or @code{if}
## statement.  For example, statements like
##
## @example
## @group
## while (c = getc())
##   @dots{}
## @end group
## @end example
##
## @noindent
## are common in C programming.
##
## It is possible to avoid all warnings about such statements by
## disabling the @code{Octave:assign-as-truth-value} warning,
## but that may also let real errors like
##
## @example
## @group
## if (x = 1)  # intended to test (x == 1)!
##   @dots{}
## @end group
## @end example
##
## @noindent
## slip by.
##
## In such cases, it is possible suppress errors for specific statements by
## writing them with an extra set of parentheses.  For example, writing the
## previous example as
##
## @example
## @group
## while ((c = getc()))
##   @dots{}
## @end group
## @end example
##
## @noindent
## will prevent the warning from being printed for this statement, while
## allowing Octave to warn about other assignments used in conditional
## contexts.
##
## By default, the @code{Octave:assign-as-truth-value} warning is enabled.
##
## @item Octave:associativity-change
## If the @code{Octave:associativity-change} warning is
## enabled, Octave will warn about possible changes in the meaning of
## some code due to changes in associativity for some operators.
## Associativity changes have typically been made for @sc{matlab}
## compatibility.  By default, the @code{Octave:associativity-change}
## warning is enabled.
##
## @item Octave:divide-by-zero
## If the @code{Octave:divide-by-zero} warning is enabled, a
## warning is issued when Octave encounters a division by zero.  By
## default, the @code{Octave:divide-by-zero} warning is enabled.
##
## @item Octave:empty-list-elements
## If the @code{Octave:empty-list-elements} warning is enabled, a
## warning is issued when an empty matrix is found in a matrix list.
## For example:
##
## @example
## a = [1, [], 3, [], 5]
## @end example
##
## @noindent
## By default, the @code{Octave:empty-list-elements} warning is enabled.
##
## @item Octave:fortran-indexing
## If the @code{Octave:fortran-indexing} warning is enabled, a warning is
## printed for expressions which select elements of a two-dimensional matrix
## using a single index.  By default, the @code{Octave:fortran-indexing}
## warning is disabled.
##
## @item Octave:function-name-clash
## If the @code{Octave:function-name-clash} warning is enabled, a
## warning is issued when Octave finds that the name of a function
## defined in a function file differs from the name of the file.  (If
## the names disagree, the name declared inside the file is ignored.)
## By default, the @code{Octave:function-name-clash} warning is enabled.
##
## @item Octave:future-time-stamp
## If the @code{Octave:future-time-stamp} warning is enabled, Octave
## will print a warning if it finds a function file with a time stamp
## that is in the future.  By default, the
## @code{Octave:future-time-stamp} warning is enabled.
##
## @item Octave:imag-to-real
## If the @code{Octave:imag-to-real} warning is enabled, a warning is
## printed for implicit conversions of complex numbers to real numbers.
## By default, the @code{Octave:imag-to-real} warning is disabled.
##
## @item Octave:matlab-incompatible
## Print warnings for Octave language features that may cause
## compatibility problems with @sc{matlab}.
##
## @item Octave:missing-semicolon
## If the @code{Octave:missing-semicolon} warning is enabled, Octave
## will warn when statements in function definitions don't end in
## semicolons.  By default the @code{Octave:missing-semicolon} warning
## is disabled.
##
## @item Octave:neg-dim-as-zero
## If the @code{Octave:neg-dim-as-zero} warning is enabled, print a warning
## for expressions like
##
## @example
## eye (-1)
## @end example
##
## @noindent
## By default, the @code{Octave:neg-dim-as-zero} warning is disabled.
##
## @item Octave:num-to-str
## If the @code{Octave:num-to-str} warning is enable, a warning is
## printed for implicit conversions of numbers to their ASCII character
## equivalents when strings are constructed using a mixture of strings and
## numbers in matrix notation.  For example,
##
## @example
## @group
## [ "f", 111, 111 ]
##      @result{} "foo"
## @end group
## @end example
##
## @noindent
## elicits a warning if the @code{Octave:num-to-str} warning is
## enabled.  By default, the @code{Octave:num-to-str} warning is enabled.
##
## @item Octave:possible-matlab-short-circuit-operator
## If the @code{Octave:possible-matlab-short-circuit-operator} warning
## is enabled, Octave will warn about using the not short circuiting
## operators @code{&} and @code{|} inside @code{if} or @code{while}
## conditions. They normally never short circuit, but @sc{Matlab} always
## short circuits if any logical operators are used in a condition. You
## can turn on the option
##
## @example
## @group
## do_braindead_shortcircuit_evaluation(1)
## @end group
## @end example
##
## @noindent
## if you would like to enable this short-circuit evaluation in
## Octave. Note that the @code{&&} and @code{||} operators always short
## circuit in both Octave and @sc{Matlab}, so it's only necessary to
## enable @sc{Matlab}-style short-circuiting it's too arduous to modify
## existing code that relies on this behaviour.
##
## @item Octave:precedence-change
## If the @code{Octave:precedence-change} warning is enabled, Octave
## will warn about possible changes in the meaning of some code due to
## changes in precedence for some operators.  Precedence changes have
## typically been made for @sc{matlab} compatibility.  By default, the
## @code{Octave:precedence-change} warning is enabled.
##
## @item Octave:reload-forces-clear
## If several functions have been loaded from the same file, Octave must
## clear all the functions before any one of them can be reloaded.  If
## the @code{Octave:reload-forces-clear} warning is enabled, Octave will
## warn you when this happens, and print a list of the additional
## functions that it is forced to clear.  By default, the
## @code{Octave:reload-forces-clear} warning is enabled.
##
## @item Octave:resize-on-range-error
## If the @code{Octave:resize-on-range-error} warning is enabled, print a
## warning when a matrix is resized by an indexed assignment with
## indices outside the current bounds.  By default, the
## @code{Octave:resize-on-range-error} warning is disabled.
##
## @item Octave:separator-insert
## Print warning if commas or semicolons might be inserted
## automatically in literal matrices.
##
## @item Octave:single-quote-string
## Print warning if a single quote character is used to introduce a
## string constant.
##
## @item Octave:str-to-num
## If the @code{Octave:str-to-num} warning is enabled, a warning is printed
## for implicit conversions of strings to their numeric ASCII equivalents.
## For example,
##
## @example
## @group
## "abc" + 0
##      @result{} 97 98 99
## @end group
## @end example
##
## @noindent
## elicits a warning if the @code{Octave:str-to-num} warning is enabled.
## By default, the @code{Octave:str-to-num} warning is disabled.
##
## @item Octave:string-concat
## If the @code{Octave:string-concat} warning is enabled, print a
## warning when concatenating a mixture of double and single quoted strings.
## By default, the @code{Octave:string-concat} warning is disabled.
##
## @item Octave:undefined-return-values
## If the @code{Octave:undefined-return-values} warning is disabled,
## print a warning if a function does not define all the values in
## the return list which are expected.  By default, the
## @code{Octave:undefined-return-values} warning is enabled.
##
## @item Octave:variable-switch-label
## If the @code{Octave:variable-switch-label} warning is enabled, Octave
## will print a warning if a switch label is not a constant or constant
## expression.  By default, the @code{Octave:variable-switch-label}
## warning is disabled.
## @end table

function warning_ids ()
  help ("warning_ids");
endfunction
