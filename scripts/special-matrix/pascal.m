########################################################################
##
## Copyright (C) 1999-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{P} =} pascal (@var{n})
## @deftypefnx {} {@var{P} =} pascal (@var{n}, @var{t})
## Return the Pascal matrix of order @var{n} if @code{@var{t} = 0}.
##
## The default value of @var{t} is 0.
##
## When @code{@var{t} = 1}, return the pseudo-lower triangular
## Cholesky@tie{}factor of the Pascal matrix (The sign of some columns may be
## negative).  This matrix is its own inverse, that is
## @code{pascal (@var{n}, 1) ^ 2 == eye (@var{n})}.
##
## If @code{@var{t} = -1}, return the true Cholesky@tie{}factor with strictly
## positive values on the diagonal.
##
## If @code{@var{t} = 2}, return a transposed and permuted version of
## @code{pascal (@var{n}, 1)}, which is the cube root of the identity matrix.
## That is, @code{pascal (@var{n}, 2) ^ 3 == eye (@var{n})}.
##
## @seealso{chol}
## @end deftypefn

function P = pascal (n, t = 0)

  if (nargin < 1)
    print_usage ();
  elseif (! (isscalar (n) && isscalar (t)))
    error ("pascal: N and T must be scalars");
  elseif (! any (t == [-1, 0, 1, 2]))
    error ("pascal: T must be -1, 0, 1, or 2, found %d", t);
  endif

  P = zeros (n);
  if (n > 0)
    P(:,1) = 1;
  endif

  if (t == -1)
    for j = 2:n
      P(j:n,j) = cumsum (P(j-1:n-1,j-1));
    endfor
  else
    for j = 2:n
      P(j:n,j) = -cumsum (P(j-1:n-1,j-1));
    endfor
  endif

  if (t == 0)
    P = P*P';
  elseif (t == 2)
    P = rot90 (P, 3);
    if (rem (n,2) != 1)
      P *= -1;
    endif
  endif

endfunction


%!assert (pascal (3,-1), [1,0,0;1,1,0;1,2,1])
%!assert (pascal (3,0), [1,1,1;1,2,3;1,3,6])
%!assert (pascal (3,0), pascal (3))
%!assert (pascal (3,1), [1,0,0;1,-1,0;1,-2,1])
%!assert (pascal (3,2), [1,1,1;-2,-1,0;1,0,0])
%!assert (pascal (0,2), [])

## Test input validation
%!error <Invalid call> pascal ()
%!error <N and T must be scalars> pascal ([1 2])
%!error <N and T must be scalars> pascal (1, [1 2])
%!error <T must be -1> pascal (3,-2)
%!error <T must be .* or 2> pascal (3,4)
