########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{y} =} shift (@var{x}, @var{b})
## @deftypefnx {} {@var{y} =} shift (@var{x}, @var{b}, @var{dim})
##
## @code{shift} is deprecated and will be removed in Octave version 10.  Use
## @code{circshift} instead.
##
## If @var{x} is a vector, perform a circular shift of length @var{b} of
## the elements of @var{x}.
##
## If @var{x} is a matrix, do the same for each column of @var{x}.
##
## If the optional @var{dim} argument is given, operate along this dimension.
## @seealso{circshift}
## @end deftypefn

## FIXME: DEPRECATED: Remove in version 10.

function y = shift (x, b, dim)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "shift is deprecated and will be removed from a future version of Octave, please use circshift instead\n");
  endif

  if (nargin < 2)
    print_usage ();
  endif

  if (numel (x) < 1)
    error ("shift: X must not be empty");
  elseif (! (isscalar (b) && b == fix (b)))
    error ("shift: B must be an integer");
  endif

  nd = ndims (x);
  sz = size (x);

  if (nargin == 3)
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("shift: DIM must be an integer and a valid dimension");
    endif
  else
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  endif

  d = sz(dim);

  idx = repmat ({':'}, nd, 1);
  if (b > 0)
    b = rem (b, d);
    idx{dim} = [d-b+1:d, 1:d-b];
  elseif (b < 0)
    b = rem (abs (b), d);
    idx{dim} = [b+1:d, 1:b];
  endif

  y = x(idx{:});

endfunction


%!test
%! a = [1, 2, 3];
%! b = [4, 5, 6];
%! c = [7, 8, 9];
%!
%! r = [a, b, c];
%! m = [a; b; c];
%!
%! assert (shift (r, 0), r);
%! assert (shift (r, 3), [c, a, b]);
%! assert (shift (r, -6), [c, a, b]);
%! assert (shift (r, -3), [b, c, a]);
%! assert (shift (m, 1), [c; a; b]);
%! assert (shift (m, -2), [c; a; b]);

## Test input validation
%!error <Invalid call> shift ()
%!error <Invalid call> shift (1)
%!error <X must not be empty> shift ([], 1)
%!error <B must be an integer> shift (ones (2), ones (2))
%!error <B must be an integer> shift (ones (2), 1.5)
%!error <DIM must be an integer> shift (1, 1, 1.5)
%!error <DIM must be .* a valid dimension> shift (1, 1, 0)
%!error <DIM must be .* a valid dimension> shift (1, 1, 3)
