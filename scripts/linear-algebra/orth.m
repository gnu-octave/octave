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
## @deftypefn  {} {@var{B} =} orth (@var{A})
## @deftypefnx {} {@var{B} =} orth (@var{A}, @var{tol})
## Return an orthonormal basis of the range space of @var{A}.
##
## The dimension of the range space is taken as the number of singular values
## of @var{A} greater than @var{tol}.  If the argument @var{tol} is missing, it
## is computed as
##
## @example
## max (size (@var{A})) * max (svd (@var{A})) * eps
## @end example
## @seealso{null}
## @end deftypefn

function B = orth (A, tol)

  if (nargin < 1)
    print_usage ();
  endif

  if (isempty (A))
    B = [];
    return;
  endif

  [U, S, V] = svd (A);

  [rows, cols] = size (A);

  [S_nr, S_nc] = size (S);

  if (S_nr == 1 || S_nc == 1)
    s = S(1);
  else
    s = diag (S);
  endif

  if (nargin == 1)
    if (isa (A, "single"))
      tol = max (size (A)) * s (1) * eps ("single");
    else
      tol = max (size (A)) * s (1) * eps;
    endif
  endif

  rank = sum (s > tol);

  if (rank > 0)
    B = -U(:, 1:rank);
  else
    B = zeros (rows, 0);
  endif

endfunction


%!test
%! for i = 1:20
%!   A = rand (10, 10);
%!   V = orth (A);
%!   if (det (A) != 0)
%!     assert (V'*V, eye (10), 100*eps);
%!   endif
%! endfor
