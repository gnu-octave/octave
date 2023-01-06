########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
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
## @deftypefn  {} {[@var{s}, @var{l}] =} bounds (@var{x})
## @deftypefnx {} {[@var{s}, @var{l}] =} bounds (@var{x}, @var{dim})
## @deftypefnx {} {[@var{s}, @var{l}] =} bounds (@dots{}, "nanflag")
## Return the smallest and largest values of the input data @var{x}.
##
## If @var{x} is a vector, the bounds are calculated over the elements of
## @var{x}.  If @var{x} is a matrix, the bounds are calculated for each column.
## For a multi-dimensional array, the bounds are calculated over the first
## non-singleton dimension.
##
## If the optional argument @var{dim} is given, operate along this dimension.
##
## The optional argument @qcode{"nanflag"} defaults to @qcode{"omitnan"} which
## does not include NaN values in the result.  If the argument
## @qcode{"includenan"} is given, and there is a NaN present, then the result
## for both smallest (@var{s}) and largest (@var{l}) elements will be NaN.
##
## The bounds are a quickly computed measure of the dispersion of a data set,
## but are less accurate than @code{iqr} if there are outlying data points.
## @seealso{range, iqr, mad, std}
## @end deftypefn

function [s, l] = bounds (x, dim, nanflag = false)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("bounds: X must be a numeric vector or matrix");
  endif

  need_dim = true;
  if (nargin == 2)
    if (ischar (dim))
      nanflag = dim;
    else
      need_dim = false;
    endif
  elseif (nargin == 3)
    need_dim = ifelse (isempty (dim), true, false);
  endif

  sz = size (x);
  if (need_dim)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (! (isscalar (dim) && dim == fix (dim) && dim > 0))
      error ("bounds: DIM must be an integer and a valid dimension");
    endif
  endif

  if (nanflag)
    nanflag = strcmp (nanflag, "includenan");
  endif

  s = min (x, [], dim);
  l = max (x, [], dim);
  if (nanflag)
    nanidx = any (isnan (x), dim);
    s(nanidx) = NaN;
    l(nanidx) = NaN;
  endif

endfunction


%!test
%! [s,l] = bounds (1:10);
%! assert ([s,l], [1, 10]);
%!test
%! [s,l] = bounds ([10:-1:1]');
%! assert ([s,l], [1, 10]);
%!test
%! [s,l] = bounds (single (1:10));
%! assert ([s,l], single ([1, 10]));
%!assert (bounds (magic (3)), [3, 1, 2])
%!assert (bounds (magic (3), 2), [1; 3; 2])
%!test
%! x = magic (3);
%! x(2,3) = NaN;
%! assert (bounds (x), [3, 1, 2]);
%! assert (bounds (x, "omitnan"), [3, 1, 2]);
%! assert (bounds (x, "includenan"), [3, 1, NaN]);
%! assert (bounds (x, 2), [1; 3; 2]);
%! assert (bounds (x, 2, "omitnan"), [1; 3; 2]);
%! assert (bounds (x, 2, "includenan"), [1; NaN; 2]);
%!test
%! x = reshape (1:27, [3, 3, 3]);
%! [s,l] = bounds (x, 3);
%! assert (s, x(:,:,1));
%! assert (l, x(:,:,3));

## Test input validation
%!error <Invalid call> bounds ()
%!error <X must be a numeric> bounds (['A'; 'B'])
%!error <DIM must be an integer> bounds (1, ones (2,2))
%!error <DIM must be an integer> bounds (1, 1.5)
%!error <DIM must be .* a valid dimension> bounds (1, 0)
