########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{q} =} cumtrapz (@var{y})
## @deftypefnx {} {@var{q} =} cumtrapz (@var{x}, @var{y})
## @deftypefnx {} {@var{q} =} cumtrapz (@dots{}, @var{dim})
## Cumulative numerical integration of points @var{y} using the trapezoidal
## method.
##
## @w{@code{cumtrapz (@var{y})}} computes the cumulative integral of @var{y}
## along the first non-singleton dimension.  Where @code{trapz} reports only
## the overall integral sum, @code{cumtrapz} reports the current partial sum
## value at each point of @var{y}.
##
## When the argument @var{x} is omitted an equally spaced @var{x} vector with
## unit spacing (1) is assumed.  @code{cumtrapz (@var{x}, @var{y})} evaluates
## the integral with respect to the spacing in @var{x} and the values in
## @var{y}.  This is useful if the points in @var{y} have been sampled
## unevenly.
##
## If the optional @var{dim} argument is given, operate along this dimension.
##
## Application Note: If @var{x} is not specified then unit spacing will be
## used.  To scale the integral to the correct value you must multiply by the
## actual spacing value (deltaX).
## @seealso{trapz, cumsum}
## @end deftypefn

function z = cumtrapz (x, y, dim)

  if (nargin < 1)
    print_usage ();
  endif

  have_xy = have_dim = false;

  if (nargin == 3)
    have_xy = true;
    have_dim = true;
  elseif (nargin == 2)
    if (isscalar (y) && ! isscalar (x))
      have_dim = true;
      dim = y;
    else
      have_xy = true;
    endif
  endif

  if (have_xy)
    nd = ndims (y);
    sz = size (y);
  else
    nd = ndims (x);
    sz = size (x);
  endif

  if (! have_dim)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("trapz: DIM must be an integer and a valid dimension");
    endif
  endif

  n = sz(dim);
  idx1 = idx2 = {':'}(ones (nd, 1));  # repmat ({':'}, [nd, 1]), but faster
  idx1{dim} = 2 : n;
  idx2{dim} = 1 : (n - 1);

  if (! have_xy)
    z = 0.5 * cumsum (x(idx1{:}) + x(idx2{:}), dim);
  elseif (isscalar (x))
    z = x * 0.5 * cumsum (y(idx1{:}) + y(idx2{:}), dim);
  elseif (isvector (x))
    if (length (x) != n)
      error ("cumtrapz: length of X and length of Y along DIM must match");
    endif
      ## Reshape vector to point along dimension DIM
      shape = ones (nd, 1);
      shape(dim) = n;
      x = reshape (x, shape);
      z = 0.5 * cumsum (diff (x) .* (y(idx1{:}) + y(idx2{:})), dim);
  else
    if (! size_equal (x, y))
      error ("cumtrapz: X and Y must have same shape");
    endif
    z = 0.5 * cumsum (diff (x, 1, dim) .* (y(idx1{:}) + y(idx2{:})), dim);
  endif

  sz(dim) = 1;
  z = cat (dim, zeros (sz), z);

endfunction


%!shared x1, x2, y
%! x1 = [1:5];
%! x2 = [2:2:10];
%! y = [1:5];
%!
%!assert (cumtrapz (y), [0, 1.5, 4, 7.5, 12])
%!assert (cumtrapz (y'), [0, 1.5, 4, 7.5, 12]')
%!assert (cumtrapz (1, y), [0, 1.5, 4, 7.5, 12])
%!assert (cumtrapz (2, y), [0, 3, 8, 15, 24])
%!assert (cumtrapz (x1, y),[0, 1.5, 4, 7.5, 12])
%!assert (cumtrapz (x2, y),[0, 3, 8, 15, 24])
%!assert (cumtrapz (2, y, 2), [0, 3, 8, 15, 24])
%!assert (cumtrapz (x2, y, 2), [0, 3, 8, 15, 24])
%!assert (cumtrapz (y, 1), [0, 0, 0, 0, 0])
%!assert (cumtrapz (2, y, 1), [0, 0, 0, 0, 0])
%!assert (cumtrapz (y', 2), [0, 0, 0, 0, 0]')

%!shared x1, x2, y
%! x1 = [0,0,0;2,2,2];
%! x2 = [0,2,4;0,2,4];
%! y = [1,2,3;4,5,6];
%!
%!assert (cumtrapz (y), [0,0,0;2.5,3.5,4.5])
%!assert (cumtrapz (x1, y), [0,0,0;5,7,9])
%!assert (cumtrapz (y, 1), [0,0,0;2.5,3.5,4.5])
%!assert (cumtrapz (x1, y, 1), [0,0,0;5,7,9])
%!assert (cumtrapz (y, 2), [0,1.5,4;0,4.5,10])
%!assert (cumtrapz (x2, y, 2), [0,3,8;0,9,20])

## Test ND-array implementation
%!shared x1,x2,y
%! x1 = 1:3;
%! x2 = reshape ([0,2,4;0,2,4], [1 2 3]);
%! y = reshape ([1,2,3;4,5,6], [1 2 3]);
%!
%!assert (cumtrapz (y,3), reshape ([0,1.5,4;0,4.5,10],[1 2 3]))
%!assert (cumtrapz (x1,y,3), reshape ([0,1.5,4;0,4.5,10],[1 2 3]))
%!assert (cumtrapz (x2,y,3), reshape ([0,3,8;0,9,20],[1 2 3]))

## Test input validation
%!error <Invalid call> cumtrapz ()
%!error <DIM must be an integer> cumtrapz (1, 2, [1 2])
%!error <DIM must be an integer> cumtrapz (1, 2, 1.5)
%!error <DIM must be .* a valid dimension> cumtrapz (1, 2, 0)
%!error <length of X and length of Y.*must match> cumtrapz ([1 2], [1 2 3])
%!error <X and Y must have same shape> cumtrapz (ones (2,3), ones (2,4))

