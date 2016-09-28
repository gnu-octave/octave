## Copyright (C) 2016 Rik Wehbring
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
## @deftypefn  {} {@var{S} =} gsvd (@var{A}, @var{B})
## @deftypefnx {} {[@var{U}, @var{V}, @var{X}, @var{C}, @var{S}] =} gsvd (@var{A}, @var{B})
## Compute the generalized singular value decomposition of (@var{A}, @var{B}):
##
## @tex
## $$ A = U C X^\dagger $$
## $$ B = V S X^\dagger $$
## $$ C^\dagger C + S^\dagger S = eye (columns (A)) $$
## @end tex
## @ifnottex
##
## @example
## @group
## A = U*C*X'
## B = V*S*X'
## C'*C + S'*S = eye (columns (A))
## @end group
## @end example
##
## @end ifnottex
##
## The function @code{gsvd} normally returns the vector of generalized singular
## values
## @tex
## $$ \sqrt{{{diag (C^\dagger C)} \over {diag (S^\dagger S)}}} $$
## @end tex
## @ifnottex
## @code{sqrt (diag (C'*C) ./ diag (S'*S))}.
## @end ifnottex
## If asked for five return values, it also computes
## @tex
## $U$, $V$, and $X$.
## @end tex
## @ifnottex
## U, V, and X.
## @end ifnottex
##
## The code is a wrapper to the corresponding @sc{lapack} dggsvd and zggsvd
## routines.
##
## @seealso{svd}
## @end deftypefn

## FIXME: This m-file is a wrapper around __gsvd__ in libinterp/corefcn.
## It was put in place strictly for the 4.2.0 release in order to achieve
## Matlab-compatible output for the gsvd function.  Eventually the C++ code
## needs to be updated to reflect what is being done in this m-file and then
## this m-file should be deleted.

function [U, V, X, C, S] = gsvd (A, B, econ = false)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 3)
    warning ('gsvd: "economy" option is not yet implemented');
  endif

  if (nargout <= 1)
    U = __gsvd__ (A, B);
  else
    [U, V, X, C, S, R] = __gsvd__ (A, B);
    X = (R / X)';
    [m, n] = size (A);
    if (m > n)
      C = [C; zeros(m-n, n)];
    elseif (m < n)
      C = [C, zeros(m, n-m)];
      S0 = S;
      S = eye (n);
      S(1:m, 1:m) = S0;
    endif
  endif

endfunction


## FIXME: All BIST tests are in the C++ file __gsvd__.cc.  They are currently
## not run because they have not been updated to reflect the actual expected
## output.
