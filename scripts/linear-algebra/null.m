########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{Z} =} null (@var{A})
## @deftypefnx {} {@var{Z} =} null (@var{A}, @var{tol})
## Return an orthonormal basis @var{Z} of the null space of @var{A}.
##
## The dimension of the null space @var{Z} is taken as the number of singular
## values of @var{A} not greater than @var{tol}.  If the argument @var{tol}
## is missing, it is computed as
##
## @example
## max (size (@var{A})) * max (svd (@var{A}, 0)) * eps
## @end example
## @seealso{orth, svd}
## @end deftypefn

function Z = null (A, tol)

  if (nargin < 1)
    print_usage ();
  elseif (nargin == 2 && strcmp (tol, "r"))
    error ("null: option for rational not yet implemented");
  endif

  [~, S, V] = svd (A, 0);  # Use economy-sized svd if possible.

  if (isempty (A))
    ## In case of A = [], zeros (0,X), zeros (X,0) Matlab R2020b seems to
    ## simply return the nullspace "V" of the svd-decomposition (bug #59630).
    Z = V;
  else
    out_cls = class (V);

    ## Extract column vector from Diagonal Matrix which depends on size
    if (rows (S) > 1)
      s = diag (S);
    else
      s = S(1);
    endif
    if (nargin == 1)
      tol = max (size (A)) * s(1) * eps (out_cls);
    endif
    rank = sum (s > tol);

    cols = columns (A);
    if (rank < cols)
      Z = V(:, rank+1:cols);
      Z(abs (Z) < eps (out_cls)) = 0;
    else
      Z = zeros (cols, 0, out_cls);
    endif
  endif

endfunction


## Exact tests
%!test
%! A = {
%!   [], [];
%!   zeros(1,0), [];
%!   zeros(4,0), [];
%!   zeros(0,1), 1;
%!   zeros(0,4), eye(4);
%!   0, 1;
%!   1, zeros(1,0);
%!   [1 0; 0 1], zeros(2,0);
%!   [1 0; 1 0], [0 1]';
%! };
%! for i = 1:rows (A)
%!   assert (null (A{i,1}), A{i,2});
%!   assert (null (single (A{i,1})), single (A{i,2}));
%! endfor

## Inexact tests
%!test
%! A = {
%!   [1 1; 0 0], [-1/sqrt(2) 1/sqrt(2)]';
%! };
%! for i = 1:rows (A)
%!   assert (null (A{i,1}), A{i,2}, eps);
%!   assert (null (single (A{i,1})), single (A{i,2}), eps);
%! endfor

## Tests with tolerance input
%!test
%! tol = 1e-4;
%! A = {
%!   @(e) [1 0; 0 tol-e], [0 1]';
%!   @(e) [1 0; 0 tol+e], zeros(2,0);
%! };
%! for i = 1:rows (A)
%!   assert (null (A{i,1}(eps ("double")), tol), A{i,2});
%!   assert (null (single (A{i,1}(eps ("single"))), tol), single (A{i,2}));
%! endfor

## Input corner cases
%!assert (null (uint8 ([])), [])

## Test input validation
%!error <Invalid call> null ()
%!error <rational not yet implemented> null (1, 'r')
