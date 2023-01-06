########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @documentencoding UTF-8
## @defvr {Documentation} slash
## Backslash and slash perform various sorts of division.
##
## @table @asis
## @item @code{\} Matrix Left Division (``backslash'')
## Solve systems of equations
## @iftex
## $Ax = b$ for $x$.
## @end iftex
## @ifnottex
## Ax = b for x.
## @end ifnottex
## @code{A \ b} is conceptually equivalent to @code{inv (A) * b} but is
## computed more efficiently and accurately, without forming the matrix
## inverse directly.
##
## Example:
##
## @example
## @group
## A = [1 2; 3 4];
## b = [4; 2];
## x = A \ b
##   @result{} x =
##        -6
##         5
## isequal (A*x, b)
##   @result{} 1
## @end group
## @end example
##
## If the system is not square, or if the matrix is singular, a minimum-norm
## solution is computed (@code{norm (A*x - b)}).
##
## For dense matrices, backslash uses the Gaussian Elimination algorithm
## with partial pivoting.  For sparse matrices, backslash uses a direct
## method to compute an LU factorization (@pxref{XREFlu,,@code{lu}}).  The
## direct method tries to minimize ``fill-in'' of zeros but it could
## nonetheless use a lot of memory; if this is a concern, consider an iterative
## method (@pxref{XREFcgs,,@code{cgs}} or @pxref{XREFgmres,,@code{gmres}}).
##
## @item @code{/} Matrix Right Division
## The forward slash notation can be used to solve systems of the form
## @iftex
## $AB = C$ for $A$
## @end iftex
## @ifnottex
## AB = C for A
## @end ifnottex
## using @code{A = C / B}.
##
## @item @code{./} and @code{.\} Component-wise ``Hadamard'' Division
## The ``dot slash'' operators perform element-by-element division, for
## example:
##
## @example
## @group
## A = [1 10 12; 24 30 42];
## B = [1 5 4; 6 6 7];
## A ./ B
##   @result{}
##        1   2   3
##        4   5   6
## @end group
## @end example
##
## If the sizes are not the same, ``broadcasting'' may apply
## (@pxref{Broadcasting}):
##
## @example
## @group
## 420 ./ B
##   @result{}
##        420    84   105
##         70    70    60
## [20; 42] ./ B
##   @result{}
##        20    4    5
##         7    7    6
## @end group
## @end example
##
## @end table
##
## @seealso{mldivide, mrdivide, ldivide, rdivide, linsolve}
## @end defvr


## Mark file as being tested.  No real test needed for a documentation .m file
%!assert (1)
