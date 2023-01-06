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

## -*- texinfo -*-
## @cindex warning ids
##
## @table @code
## @item Octave:abbreviated-property-match
## If the @code{Octave:abbreviated-property-match} warning is enabled, a
## warning is printed if a non-exact or ambiguous match is being used for a
## operation specifying an object property.  For example, for a graphics
## object, @var{fig}, with the property @qcode{'displayname'},
## @code{get (@var{fig}, 'dis')} elicits a warning if the
## @code{Octave:abbreviated-property-match} warning is enabled.
## By default, the @code{Octave:abbreviated-property-match} warning is enabled.
##
## @item Octave:addpath-pkg
## If the @code{Octave:addpath-pkg} warning is enabled,
## Octave will warn when a package directory (i.e., +package_name) is added
## to the @code{path}.  Typically, only the parent directory which contains the
## package directory should be added to the load path.
## By default, the @code{Octave:addpath-pkg} warning is enabled.
##
## @item Octave:array-as-logical
## If the @code{Octave:array-as-logical} warning is enabled,
## Octave will warn when an array of size greater than 1x1 is used
## as a truth value in an if, while, or until statement.
## By default, the @code{Octave:array-as-logical} warning is disabled.
##
## @item Octave:array-to-scalar
## If the @code{Octave:array-to-scalar} warning is enabled, Octave will
## warn when an implicit conversion from an array to a scalar value is
## attempted.
## By default, the @code{Octave:array-to-scalar} warning is disabled.
##
## @item Octave:array-to-vector
## If the @code{Octave:array-to-vector} warning is enabled, Octave will
## warn when an implicit conversion from an array to a vector value is
## attempted.
## By default, the @code{Octave:array-to-vector} warning is disabled.
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
## while (c = getc ())
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
## while ((c = getc ()))
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
## @item Octave:autoload-relative-file-name
## If the @code{Octave:autoload-relative-file-name} is enabled,
## Octave will warn when parsing autoload() function calls with relative
## paths to function files.  This usually happens when using autoload()
## calls in PKG_ADD files, when the PKG_ADD file is not in the same
## directory as the .oct file referred to by the autoload() command.
## By default, the @code{Octave:autoload-relative-file-name} warning is
## enabled.
##
## @item Octave:charmat-truncated
## If the @code{Octave:charmat-truncated} warning is enabled, a warning is
## printed when a character matrix with multiple rows is converted to a string.
## In this case, the Octave interpreter keeps only the first row and discards
## the others.
## By default, the @code{Octave:charmat-truncated} warning is enabled.
##
## @item Octave:classdef-to-struct
## If the @code{Octave:classdef-to-struct} warning is enabled, a warning
## is issued when a classdef object is forcibly converted into a struct with
## @code{struct (@var{CLASSDEF_OBJ})}.  Conversion removes the access
## restrictions from the object and makes private and protected properties
## visible.
## By default, the @code{Octave:classdef-to-struct} warning is enabled.
##
## @item Octave:colon-complex-argument
## If the @code{Octave:colon-complex-argument} warning is enabled, a warning
## is issued when one of the three arguments to the colon operator (base,
## increment, limit) is a complex value.  For example, @code{1:3*i} will
## cause a warning to be emitted.
## By default, the @code{Octave:colon-complex-argument} warning is enabled.
##
## @item Octave:colon-nonscalar-argument
## If the @code{Octave:colon-nonscalar-argument} warning is enabled, a warning
## is issued when one of the three arguments to the colon operator (base,
## increment, limit) is not a scalar.  For example, @code{1:[3, 5]} will
## cause a warning to be emitted.
## By default, the @code{Octave:colon-nonscalar-argument} warning is enabled.
##
## @item Octave:data-file-in-path
## If the @code{Octave:data-file-in-path} warning is enabled, a warning is
## issued when Octave does not find the target of a file operation such as
## @code{load} or @code{fopen} directly, but is able to locate the file in
## Octave's search @code{path} for files.  The warning could indicate that a
## different file target than the programmer intended is being used.
## By default, the @code{Octave:data-file-in-path} warning is enabled.
##
## @item Octave:datevec:date-format-spec
## If the @code{Octave:datevec:date-format-spec} warning is enabled, a warning
## is printed if the date format specification contains questionable date or
## time specifiers.  Typical bad patterns are using uppercase date specifiers
## or lowercase time specifiers.
## By default, the @code{Octave:datevec:date-format-spec} warning is enabled.
##
## @item Octave:deprecated-function
## If the @code{Octave:deprecated-function} warning is enabled, a
## warning is issued when Octave encounters a function that is obsolete and
## scheduled for removal from Octave.
## By default, the @code{Octave:deprecated-function} warning is enabled.
##
## @item Octave:deprecated-keyword
## If the @code{Octave:deprecated-keyword} warning is enabled, a
## warning is issued when Octave encounters a keyword that is obsolete and
## scheduled for removal from Octave.
## By default, the @code{Octave:deprecated-keyword} warning is enabled.
##
## @item Octave:deprecated-option
## If the @code{Octave:deprecated-option} warning is enabled, a
## warning is issued when an obsolete option or input to a function is used.
## By default, the @code{Octave:deprecated-option} warning is enabled.
##
## @item Octave:deprecated-property
## If the @code{Octave:deprecated-property} warning is enabled, a
## warning is issued when Octave encounters a graphics property that
## is obsolete and scheduled for removal from Octave.
## By default, the @code{Octave:deprecated-property} warning is enabled.
##
## @item Octave:eigs:UnconvergedEigenvalues
## If the @code{Octave:eigs:UnconvergedEigenvalues} warning is enabled then
## the eigs function will issue a warning if the number of calculated
## eigenvalues is less than the number of requested eigenvalues.
## By default, the @code{Octave:eigs:UnconvergedEigenvalues} warning is
## enabled.
##
## @item Octave:empty-index
## If the @code{Octave:empty-index} warning is enabled then Octave will emit a
## warning whenever indexing operators are used without an index, for example
## @code{@var{x}()}.
## By default, the @code{Octave:empty-index} warning is enabled.
##
## @item Octave:erase:chararray
## If the @code{Octave:erase:chararray} warning is enabled then the erase
## function will issue a warning if the input pattern is a character array
## rather than a string or cell array of strings.
## By default, the @code{Octave:erase:chararray} warning is enabled.
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
## that is in the future.
## By default, the @code{Octave:future-time-stamp} warning is enabled.
##
## @item Octave:glyph-render
## If the @code{Octave:glyph-render} warning is enabled, Octave will
## print a warning if the glyph for a character couldn't be rendered with
## the current font.
## By default, the @code{Octave:glyph-render} warning is enabled.
##
## @item Octave:imag-to-real
## If the @code{Octave:imag-to-real} warning is enabled, a warning is
## printed for implicit conversions of complex numbers to real numbers.
## By default, the @code{Octave:imag-to-real} warning is disabled.
##
## @item Octave:infinite-loop
## If the @code{Octave:infinite-loop} warning is enabled, a warning is
## printed when an infinite loop is detected such as @code{for i = 1:Inf} or
## @code{while (1)}.
## By default, the @code{Octave:infinite-loop} warning is enabled.
##
## @item Octave:language-extension
## Print warnings when using features that are unique to the Octave
## language and that may still be missing in @sc{matlab}.
## By default, the @code{Octave:language-extension} warning is disabled.
## The @option{--traditional} or @option{--braindead} startup options for
## Octave may also be of use, @pxref{Command Line Options}.
##
## @item Octave:legacy-function
## If the @code{Octave:legacy-function} warning is enabled, a
## warning is issued when Octave encounters a function that @sc{matlab} has
## suggested should be avoided.  The function may become obsolete at some
## point in the future and removed, in which case the warning will change to
## @code{Octave:deprecated-function}, and the function will continue to exist
## for two further versions of Octave before being removed.
## By default, the @code{Octave:legacy-function} warning is enabled.
##
## @item Octave:logical-conversion
## If the @code{Octave:logical-conversion} warning is enabled, a warning is
## printed if an implicit conversion of an array from numerical to boolean
## occurs and any of the elements in the array are not equal to zero or one.
## By default, the @code{Octave:logical-conversion} warning is enabled.
##
## @item Octave:lu:sparse_input
## If the @code{Octave:lu:sparse_input} warning is enabled, Octave
## will warn when the lu function is called with a sparse input and less than
## four output arguments.  In this case, sparsity-preserving column
## permutations are not performed and the result may be inaccurate.
## By default, the @code{Octave:lu:sparse_input} warning is enabled.
##
## @item Octave:missing-glyph
## If the @code{Octave:glyph-render} warning is enabled, Octave will
## print a warning if the current font doesn't provide a glyph for a
## used character.
## By default, the @code{Octave:missing-glyph} warning is enabled.
##
## @item Octave:missing-semicolon
## If the @code{Octave:missing-semicolon} warning is enabled, Octave
## will warn when statements in function definitions don't end in
## semicolons.
## By default the @code{Octave:missing-semicolon} warning is disabled.
##
## @item Octave:mixed-string-concat
## If the @code{Octave:mixed-string-concat} warning is enabled, print a
## warning when concatenating a mixture of double and single quoted strings.
## By default, the @code{Octave:mixed-string-concat} warning is disabled.
##
## @item  Octave:nearly-singular-matrix
## @itemx Octave:singular-matrix
## These warnings are emitted if a (nearly) singular matrix is inverted.
## By default, the @code{Octave:nearly-singular-matrix} and
## @code{Octave:singular-matrix} warnings are enabled.
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
## @item Octave:noninteger-range-as-index
## If the @code{Octave:noninteger-range-as-index} warning is enabled, a warning
## is printed if an array is indexed with a range that contains non-integer
## values.  For example,
##
## @example
## @group
## a = [1 2 3 4 5];
## b = 2.2:4.2
## @result{} 1.2  2.2  3.2
## a(b)
## @result{} 2 3 4
## @end group
## @end example
##
## @noindent
## elicits a warning if the @code{Octave:noninteger-range-as-index} warning is
## enabled.
## By default, the @code{Octave:noninteger-range-as-index} warning is enabled.
##
## @item Octave:num-to-str
## If the @code{Octave:num-to-str} warning is enabled, a warning is
## printed for implicit conversions of numbers to their UTF-8 encoded character
## equivalents when strings are constructed using a mixture of strings and
## numbers in matrix notation.  For example,
##
## @example
## @group
## [ "f", 111, 111 ]
## @result{} "foo"
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
## conditions.  They normally never short circuit, but they do short
## circuit when used in a condition.
## By default, the @code{Octave:possible-matlab-short-circuit-operator} warning
## is enabled.
##
## @item Octave:pow2:imaginary-ignored
## If the @code{Octave:pow2:imaginary-ignored} warning is enabled, a warning is
## printed if either input to @code{pow2} is complex.
## By default, the @code{Octave:pow2:imaginary-ignored} warning is enabled.
##
## @item Octave:recursive-path-search
## If the @code{Octave:recursive-path-search} warning is enabled, Octave
## will issue a warning if @code{addpath} is used with double trailing
## slashes.
## By default, the @code{Octave:recursive-path-search} warning is enabled.
##
## @item Octave:remove-init-dir
## The @code{path} function changes the search path that Octave uses
## to find functions.  It is possible to set the path to a value which
## excludes Octave's own built-in functions.  If the
## @code{Octave:remove-init-dir} warning is enabled then Octave will warn
## when the @code{path} function has been used in a way that may render
## Octave unworkable.
## By default, the @code{Octave:remove-init-dir} warning is enabled.
##
## @item Octave:reload-forces-clear
## If several functions have been loaded from the same file, Octave must
## clear all the functions before any one of them can be reloaded.  If
## the @code{Octave:reload-forces-clear} warning is enabled, Octave will
## warn you when this happens, and print a list of the additional
## functions that it is forced to clear.
## By default, the @code{Octave:reload-forces-clear} warning is enabled.
##
## @item Octave:separator-insert
## Print warning if commas or semicolons might be inserted
## automatically in literal matrices.
## By default, the @code{Octave:separator-insert} warning is disabled.
##
## @item Octave:shadowed-function
## If the @code{Octave:shadowed-function} warning is enabled, Octave will
## warn if a path is added to the search path that contains functions
## that shadow core functions.
## By default, the @code{Octave:shadowed-function} warning is enabled.
##
## @item Octave:single-quote-string
## Print warning if a single quote character is used to introduce a
## string constant.
## By default, the @code{Octave:single-quote-string} warning is disabled.
##
## @item Octave:sqrtm:SingularMatrix
## If the @code{Octave:sqrtm:SingularMatrix} warning is enabled, a warning is
## printed if the matrix square root function @code{sqrtm} is called with an
## input matrix that is singular.
## By default, the @code{Octave:sqrtm:SingularMatrix} warning is enabled.
##
## @item Octave:str-to-num
## If the @code{Octave:str-to-num} warning is enabled, a warning is printed
## for implicit conversions of strings to their numeric UTF-8 encoded byte
## sequences.  For example,
##
## @example
## @group
## "abc" + 0
## @result{} 97 98 99
## @end group
## @end example
##
## @noindent
## elicits a warning if the @code{Octave:str-to-num} warning is enabled.
## By default, the @code{Octave:str-to-num} warning is disabled.
##
## @item Octave:LaTeX:internal-error
## If the @code{Octave:LaTeX:internal-error} warning is enabled, a warning is
## printed whenever the @LaTeX{} renderer for text in plots encounters an
## issue.
## By default, the @code{Octave:LaTeX:internal-error} warning is enabled.
##
## @item Octave:unimplemented-matlab-functionality
## If the @code{Octave:unimplemented-matlab-functionality} warning is enabled,
## a warning is printed when a @sc{matlab} code construct is used which the
## Octave interpreter parses as valid, but for which Octave does not yet
## implement the functionality.
## By default, the @code{Octave:unimplemented-matlab-functionality} warning is
## enabled.
##
## @item Octave:variable-switch-label
## If the @code{Octave:variable-switch-label} warning is enabled, Octave
## will print a warning if a switch label is not a constant or constant
## expression.
## By default, the @code{Octave:variable-switch-label} warning is disabled.
## @end table
##

function warning_ids ()
  help ("warning_ids");
endfunction


## Mark file as tested.  No test needed for a documentation m-file.
%!assert (1)
