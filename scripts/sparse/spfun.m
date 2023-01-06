########################################################################
##
## Copyright (C) 2004-2023 The Octave Project Developers
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
## @deftypefn {} {@var{y} =} spfun (@var{f}, @var{S})
## Compute @code{f (@var{S})} for the nonzero elements of @var{S}.
##
## The input function @var{f} is applied only to the nonzero elements of
## the input matrix @var{S} which is typically sparse.  The function @var{f}
## can be passed as a string, function handle, or inline function.
##
## The output @var{y} is a sparse matrix with the same sparsity structure as
## the input @var{S}.  @code{spfun} preserves sparsity structure which is
## different than simply applying the function @var{f} to the sparse matrix
## @var{S} when @code{@var{f} (0) != 0}.
##
## Example
##
## Sparsity preserving @code{spfun} versus normal function application
##
## @example
## @group
## S = pi * speye (2,2)
## S =
##
## Compressed Column Sparse (rows = 2, cols = 2, nnz = 2 [50%])
##
##   (1, 1) -> 3.1416
##   (2, 2) -> 3.1416
##
## y = spfun (@@cos, S)
## y =
##
## Compressed Column Sparse (rows = 2, cols = 2, nnz = 2 [50%])
##
##   (1, 1) -> -1
##   (2, 2) -> -1
## @end group
##
## @group
## y = cos (S)
## y =
##
## Compressed Column Sparse (rows = 2, cols = 2, nnz = 4 [100%])
##
##   (1, 1) -> -1
##   (2, 1) -> 1
##   (1, 2) -> 1
##   (2, 2) -> -1
##
## @end group
## @end example
## @seealso{arrayfun, cellfun, structfun}
## @end deftypefn

function y = spfun (f, S)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isnumeric (S))
    error ("spfun: S must be numeric");
  endif

  [i, j, v] = find (S);
  [m, n] = size (S);

  y = sparse (i, j, feval (f, v), m, n);

endfunction


%!assert (spfun ("exp", [1,2;3,0]), sparse ([exp(1),exp(2);exp(3),0]))
%!assert (spfun ("exp", sparse ([1,2;3,0])), sparse ([exp(1),exp(2);exp(3),0]))
%!assert (spfun (@exp, [1,2;3,0]), sparse ([exp(1),exp(2);exp(3),0]))
%!assert (spfun (@exp, sparse ([1,2;3,0])), sparse ([exp(1),exp(2);exp(3),0]))

## Test input validation
%!error <Invalid call> spfun ()
%!error <Invalid call> spfun (@cos)
%!error <S must be numeric> spfun (@cos, {1})
%!error <S must be numeric> spfun (@cos, "FooBar")
