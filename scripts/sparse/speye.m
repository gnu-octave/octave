########################################################################
##
## Copyright (C) 2004-2025 The Octave Project Developers
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
## @deftypefn  {} {@var{s} =} speye ()
## @deftypefnx {} {@var{s} =} speye (@var{n})
## @deftypefnx {} {@var{s} =} speye (@var{m}, @var{n})
## @deftypefnx {} {@var{s} =} speye ([@var{m}, @var{n}])
## Return a sparse identity matrix of size @var{m}x@var{n}.
##
## If called with no arguments, return the sparse scalar value @code{1}.
##
## If invoked with a single scalar argument @var{n}, return a sparse square
## @nospell{NxN} identity matrix.
##
## If supplied two scalar arguments (@var{m}, @var{n}), or a 2-element vector
## @w{@code{[@var{m}, @var{n}]}}, return a sparse @nospell{MxN} identity matrix
## with @var{m} rows and @var{n} columns.
##
## Programming Note: The implementation is significantly more efficient than
## @w{@code{sparse (eye (@dots{}))}}@ as the full matrix is not constructed.
## @seealso{sparse, spdiags, eye}
## @end deftypefn

function s = speye (m, n)

  if (nargin == 0)
    m = n = 1;
  elseif (nargin == 1)
    if (! isvector (m) || numel (m) > 2)
      print_usage ();
    endif

    if (isscalar (m))
      n = m;
    else
      n = m(2);
      m = m(1);
    endif
  else
    if (! (isscalar (m) && isscalar (n)))
      error ("speye: M and N must be scalar dimensions");
    endif
  endif

  ## Note: Matlab compatibility requires using 0 for negative dimensions.
  m = ifelse (m < 0, 0, m);
  n = ifelse (n < 0, 0, n);
  lo = min (m, n);
  s = sparse (1:lo, 1:lo, 1, m, n);

endfunction


%!assert (speye (), sparse (1))
%!assert (speye (4), sparse (1:4,1:4,1))
%!assert (speye (2,4), sparse (1:2,1:2,1,2,4))
%!assert (speye (4,2), sparse (1:2,1:2,1,4,2))
%!assert (speye ([4,2]), sparse (1:2,1:2,1,4,2))
%!assert (speye (2, -3), sparse (2, 0))

## Test input validation
%!error <Invalid call> speye (ones (2,2))
%!error <Invalid call> speye ([1, 2, 3])
%!error <M and N must be scalar dimensions> speye ([1, 2], 3)
%!error <M and N must be scalar dimensions> speye (1, [2, 3])
