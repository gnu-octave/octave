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
## @deftypefn  {} {@var{B} =} prepad (@var{A}, @var{l})
## @deftypefnx {} {@var{B} =} prepad (@var{A}, @var{l}, @var{c})
## @deftypefnx {} {@var{B} =} prepad (@var{A}, @var{l}, @var{c}, @var{dim})
## Prepend the scalar value @var{c} to the vector @var{A} until it is of length
## @var{l}.  If @var{c} is not given, a value of 0 is used.
##
## If @code{length (@var{A}) > @var{l}}, elements from the beginning of @var{A}
## are removed until a vector of length @var{l} is obtained.
##
## If @var{A} is a matrix, elements are prepended or removed from each row.
##
## If the optional argument @var{dim} is given, operate along this dimension.
##
## If @var{dim} is larger than the dimensions of @var{A}, the result will have
## @var{dim} dimensions.
## @seealso{postpad, cat, resize}
## @end deftypefn

function B = prepad (A, l, c, dim)

  if (nargin < 2)
    print_usage ();
  endif

  if (nargin < 3 || isempty (c))
    c = 0;
  else
    if (! isscalar (c))
      error ("prepad: pad value C must be empty or a scalar");
    endif
  endif

  nd = ndims (A);
  sz = size (A);
  if (nargin < 4)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim) && dim >= 1))
      error ("prepad: DIM must be an integer and a valid dimension");
    endif
  endif

  if (! isscalar (l) || l < 0)
    error ("prepad: length L must be a positive scalar");
  endif

  if (dim > nd)
    sz(nd+1:dim) = 1;
  endif

  d = sz(dim);

  if (d == l)
    ## This optimization makes sense because the function is used to match
    ## the length between two vectors not knowing a priori is larger, and
    ## allow for:
    ##    ml = max (numel (v1), numel (v2));
    ##    v1 = prepad (v1, ml);
    ##    v2 = prepad (v2, ml);
    B = A;
  elseif (d >= l)
    idx = repmat ({':'}, nd, 1);
    idx{dim} = d-l+1:d;
    B = A(idx{:});
  else
    sz(dim) = l - d;
    B = cat (dim, c(ones (sz)), A);
  endif

endfunction


%!assert (prepad ([1,2], 4), [0,0,1,2])
%!assert (prepad ([1;2], 4), [0;0;1;2])

%!assert (prepad ([1,2], 4, 2), [2,2,1,2])
%!assert (prepad ([1;2], 4, 2), [2;2;1;2])

%!assert (prepad ([1 2], 2), [1 2])
%!assert (prepad ([1; 2], 2), [1; 2])
%!assert (prepad ([1; 2], 2, 3, 2), [3 1; 3 2])

%!assert (prepad ([1,2], 2, 2, 1), [2,2;1,2])

%!assert (prepad ([1,2], 2, 2, 3), reshape ([2,2,1,2], 1, 2, 2))
%!assert (prepad ([1;2], 2, 2, 3), reshape ([2;2;1;2], 2, 1, 2))

%! ## Test with string concatenation
%!assert <*44162> (prepad ("Octave", 16, "x"), "xxxxxxxxxxOctave")
%!assert (prepad ("Octave", 4), "tave")

## FIXME: We need tests for multidimensional arrays.

%!error <Invalid call> prepad ()
%!error <Invalid call> prepad (1)
%!error <C must be empty or a scalar> prepad ([1,2], 2, ones (2))
%!error <DIM must be an integer> prepad ([1,2], 2, 2, ones (3))
%!error <DIM must be an integer> prepad ([1,2], 2, 2, 1.1)
%!error <L must be a positive scalar> prepad ([1,2], ones (2))
%!error <L must be a positive scalar> prepad ([1,2], -1)
