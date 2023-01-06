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
## @deftypefn  {} {@var{y} =} circshift (@var{x}, @var{n})
## @deftypefnx {} {@var{y} =} circshift (@var{x}, @var{n}, @var{dim})
## Circularly shift the values of the array @var{x}.
##
## @var{n} must be a vector of integers no longer than the number of dimensions
## in @var{x}.  The values of @var{n} can be either positive or negative, which
## determines the direction in which the values of @var{x} are shifted.  If an
## element of @var{n} is zero, then the corresponding dimension of @var{x} will
## not be shifted.  If @var{n} is a scalar and no @var{dim} is specified then
## the shift is applied to the first non-singular dimension.
##
## If a scalar @var{dim} is given then operate along the specified dimension.
## In this case @var{n} must be a scalar as well.
##
## Examples:
##
## @example
## x = [1, 2, 3;
##      4, 5, 6;
##      7, 8, 9];
## ## positive shift on rows (1st non-singular dim)
## circshift (x, 1)
##   @result{}
##        7   8   9
##        1   2   3
##        4   5   6
## ## negative shift on rows (1st non-singular dim)
## circshift (x, -2)
##   @result{}
##        7   8   9
##        1   2   3
##        4   5   6
## ## no shift of rows, shift columns by 1 (2nd dimension)
## circshift (x, [0,1])
##   @result{}
##        3   1   2
##        6   4   5
##        9   7   8
## ## shift columns (2nd dimension)
## circshift (x, 1, 2)
##   @result{}
##        3   1   2
##        6   4   5
##        9   7   8
## @end example
## @seealso{permute, ipermute, shiftdim}
## @end deftypefn

function y = circshift (x, n, dim)

  if (nargin < 2)
    print_usage ();
  endif

  if (isempty (x))
    y = x;
    return;
  endif

  nd = ndims (x);
  sz = size (x);

  if (nargin == 2)
    if (isscalar (n))
      ## Find the first non-singleton dimension.
      (dim = find (sz > 1, 1)) || (dim = 1);
      n = [zeros(1, dim-1), n];
    endif
  elseif (nargin == 3)
    if ( ! isscalar (n))
      error ("circshift: N must be a scalar when DIM is also specified");
    endif
    n = [zeros(1, dim-1), n];
  endif

  if (! isvector (n) || length (n) > nd)
    error ("circshift: N must be a vector, no longer than the number of dimensions in X");
  elseif (any (n != fix (n)))
    error ("circshift: all values of N must be integers");
  endif

  idx = repmat ({':'}, 1, nd);
  for i = 1:length (n)
    b = n(i);
    d = sz(i);
    if (b > 0)
      b = rem (b, d);
      idx{i} = [d-b+1:d, 1:d-b];
    elseif (b < 0)
      b = rem (abs (b), d);
      idx{i} = [b+1:d, 1:b];
    endif
  endfor

  y = x(idx{:});

endfunction


%!shared x
%! x = [1, 2, 3; 4, 5, 6; 7, 8, 9];

%!assert (circshift (x, 1), [7, 8, 9; 1, 2, 3; 4, 5, 6])
%!assert (circshift (x, -2), [7, 8, 9; 1, 2, 3; 4, 5, 6])
%!assert (circshift (x, [0, 1]), [3, 1, 2; 6, 4, 5; 9, 7, 8])
%!assert (circshift ([], 1), [])

%!assert (circshift (eye (3), 1), circshift (eye (3), 1))
%!assert (circshift (eye (3), 1), [0,0,1;1,0,0;0,1,0])

%!assert (circshift (x, -2, 1), [7, 8, 9; 1, 2, 3; 4, 5, 6])
%!assert (circshift (x, 1, 2), [3, 1, 2; 6, 4, 5; 9, 7, 8])

%!test <*53178> assert (circshift (1:4, 1), [4 1 2 3])
%!test <*53178> assert (circshift (1:4, 1, 1), 1:4)

## Test input validation
%!error <Invalid call> circshift ()
%!error <Invalid call> circshift (1)
%!error <N must be a scalar> circshift (1, [2 3], 4)
%!error <N must be a vector> circshift (1, ones (2,2))
%!error <no longer than the number of dimensions in X> circshift (1, [1 2 3])
%!error <all values of N must be integers> circshift (1, 1.5)
