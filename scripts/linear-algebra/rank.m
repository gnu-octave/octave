########################################################################
##
## Copyright (C) 1993-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{k} =} rank (@var{A})
## @deftypefnx {} {@var{k} =} rank (@var{A}, @var{tol})
## Compute the rank of matrix @var{A}, using the singular value decomposition.
##
## The rank is taken to be the number of singular values of @var{A} that are
## greater than the specified tolerance @var{tol}.  If the second argument is
## omitted, it is taken to be
##
## @example
## tol = max (size (@var{A})) * sigma(1) * eps;
## @end example
##
## @noindent
## where @code{eps} is machine precision and @code{sigma(1)} is the largest
## singular value of @var{A}.
##
## The rank of a matrix is the number of linearly independent rows or columns
## and equals the dimension of the row and column space.  The function
## @code{orth} may be used to compute an orthonormal basis of the column space.
##
## For testing if a system @code{@var{A}*@var{x} = @var{b}} of linear equations
## is solvable, one can use
##
## @example
## rank (@var{A}) == rank ([@var{A} @var{b}])
## @end example
##
## In this case, @code{@var{x} = @var{A} \ @var{b}} finds a particular solution
## @var{x}.  The general solution is @var{x} plus the null space of matrix
## @var{A}.  The function @code{null} may be used to compute a basis of the
## null space.
##
## Example:
##
## @example
## @group
## A = [1 2 3
##      4 5 6
##      7 8 9];
## rank (A)
##   @result{} 2
## @end group
## @end example
##
## @noindent
## In this example, the number of linearly independent rows is only 2 because
## the final row is a linear combination of the first two rows:
##
## @example
## A(3,:) == -A(1,:) + 2 * A(2,:)
## @end example
##
## @seealso{null, orth, sprank, svd, eps}
## @end deftypefn

function k = rank (A, tol)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 1)
    sigma = svd (A);
    if (isempty (sigma))
      tolerance = 0;
    else
      if (isa (A, "single"))
        tolerance = max (size (A)) * sigma (1) * eps ("single");
      else
        tolerance = max (size (A)) * sigma (1) * eps;
      endif
    endif
  else
    sigma = svd (A);
    tolerance = tol;
  endif

  k = sum (sigma > tolerance);

endfunction


%!test
%! A = [1 2 3 4 5 6 7;
%!      4 5 6 7 8 9 12;
%!      1 2 3.1 4 5 6 7;
%!      2 3 4 5 6 7 8;
%!      3 4 5 6 7 8 9;
%!      4 5 6 7 8 9 10;
%!      5 6 7 8 9 10 11];
%! assert (rank (A), 4);

%!test
%! A = [1 2 3 4 5 6 7;
%!      4 5 6 7 8 9 12;
%!      1 2 3.0000001 4 5 6 7;
%!      4 5 6 7 8 9 12.00001;
%!      3 4 5 6 7 8 9;
%!      4 5 6 7 8 9 10;
%!      5 6 7 8 9 10 11];
%! assert (rank (A), 4);

%!test
%! A = [1 2 3 4 5 6 7;
%!      4 5 6 7 8 9 12;
%!      1 2 3 4 5 6 7;
%!      4 5 6 7 8 9 12.00001;
%!      3 4 5 6 7 8 9;
%!      4 5 6 7 8 9 10;
%!      5 6 7 8 9 10 11];
%! assert (rank (A), 3);

%!test
%! A = [1 2 3 4 5 6 7;
%!      4 5 6 7 8 9 12;
%!      1 2 3 4 5 6 7;
%!      4 5 6 7 8 9 12;
%!      3 4 5 6 7 8 9;
%!      4 5 6 7 8 9 10;
%!      5 6 7 8 9 10 11];
%! assert (rank (A), 3);

%!test
%! A = eye (100);
%! assert (rank (A), 100);

%!assert (rank ([]), 0)
%!assert (rank ([1:9]), 1)
%!assert (rank ([1:9]'), 1)

%!test
%! A = [1, 2, 3; 1, 2.001, 3; 1, 2, 3.0000001];
%! assert (rank (A), 3);
%! assert (rank (A,0.0009), 1);
%! assert (rank (A,0.0006), 2);
%! assert (rank (A,0.00000002), 3);
