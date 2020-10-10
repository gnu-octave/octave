########################################################################
##
## Copyright (C) 1995-2020 The Octave Project Developers
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
## @deftypefn  {} {} iqr (@var{x})
## @deftypefnx {} {} iqr (@var{x}, @var{dim})
## Return the interquartile range, i.e., the difference between the upper
## and lower quartile of the input data.
##
## If @var{x} is a matrix, do the above for first non-singleton dimension of
## @var{x}.
##
## If the optional argument @var{dim} is given, operate along this dimension.
##
## As a measure of dispersion, the interquartile range is less affected by
## outliers than either @code{range} or @code{std}.
## @seealso{bounds, mad, range, std}
## @end deftypefn

function y = iqr (x, dim)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("iqr: X must be a numeric vector or matrix");
  endif

  nd = ndims (x);
  sz = size (x);
  nel = numel (x);
  if (nargin != 2)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("iqr: DIM must be an integer and a valid dimension");
    endif
  endif

  ## This code is a bit heavy, but is needed until empirical_inv
  ## can take a matrix, rather than just a vector argument.
  n = sz(dim);
  sz(dim) = 1;
  if (isa (x, "single"))
    y = zeros (sz, "single");
  else
    y = zeros (sz);
  endif
  stride = prod (sz(1:dim-1));
  for i = 1 : nel / n
    offset = i;
    offset2 = 0;
    while (offset > stride)
      offset -= stride;
      offset2 += 1;
    endwhile
    offset += offset2 * stride * n;
    rng = [0 : n-1] * stride + offset;

    y(i) = diff (empirical_inv ([1/4, 3/4], x(rng)));
  endfor

endfunction


%!assert (iqr (1:101), 50)
%!assert (iqr (single (1:101)), single (50))

## FIXME: iqr throws horrible error when running across a dimension that is 1.
%!test
%! x = [1:100]';
%! assert (iqr (x, 1), 50);
%! assert (iqr (x', 2), 50);

%!error <Invalid call> iqr ()
%!error iqr (1)
%!error iqr (['A'; 'B'])
%!error iqr (1:10, 3)
