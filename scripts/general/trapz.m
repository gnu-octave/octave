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
## @deftypefn  {} {@var{q} =} trapz (@var{y})
## @deftypefnx {} {@var{q} =} trapz (@var{x}, @var{y})
## @deftypefnx {} {@var{q} =} trapz (@dots{}, @var{dim})
##
## Numerically evaluate the integral of points @var{y} using the trapezoidal
## method.
##
## @w{@code{trapz (@var{y})}} computes the integral of @var{y} along the first
## non-singleton dimension.  When the argument @var{x} is omitted an equally
## spaced @var{x} vector with unit spacing (1) is assumed.
## @code{trapz (@var{x}, @var{y})} evaluates the integral with respect to the
## spacing in @var{x} and the values in @var{y}.  This is useful if the points
## in @var{y} have been sampled unevenly.
##
## If the optional @var{dim} argument is given, operate along this dimension.
##
## Application Note: If @var{x} is not specified then unit spacing will be
## used.  To scale the integral to the correct value you must multiply by the
## actual spacing value (deltaX).  As an example, the integral of @math{x^3}
## over the range [0, 1] is @math{x^4/4} or 0.25.  The following code uses
## @code{trapz} to calculate the integral in three different ways.
##
## @example
## @group
## x = 0:0.1:1;
## y = x.^3;
## ## No scaling
## q = trapz (y)
##   @result{} q = 2.5250
## ## Approximation to integral by scaling
## q * 0.1
##   @result{} 0.25250
## ## Same result by specifying @var{x}
## trapz (x, y)
##   @result{} 0.25250
## @end group
## @end example
##
## @seealso{cumtrapz}
## @end deftypefn

function z = trapz (x, y, dim)

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
    z = 0.5 * sum (x(idx1{:}) + x(idx2{:}), dim);
  elseif (isscalar (x))
    z = x * 0.5 * sum (y(idx1{:}) + y(idx2{:}), dim);
  elseif (isvector (x))
    if (length (x) != n)
      error ("trapz: length of X and length of Y along DIM must match");
    endif
    ## Reshape spacing vector x to point along dimension DIM
    shape = ones (nd, 1);
    shape(dim) = n;
    x = reshape (x, shape);
    z = 0.5 * sum (diff (x) .* (y(idx1{:}) + y(idx2{:})), dim);
  else
    if (! size_equal (x, y))
      error ("trapz: X and Y must have same shape");
    endif
    z = 0.5 * sum (diff (x, 1, dim) .* (y(idx1{:}) + y(idx2{:})), dim);
  endif

endfunction


%!assert (trapz (1:5), 12)
%!assert (trapz (1, 1:5), 12)
%!assert (trapz (0.5, 1:5), 6)
%!assert (trapz ([1:5], [1:5]), 12)
%!assert (trapz ([1:5], [1:5]'), 12)
%!assert (trapz ([1:5]', [1:5]'), 12)
%!assert (trapz ([1:5]', [1:5]), 12)
%!assert (trapz (0:0.5:2,1:5), 6)
%!assert (trapz ([1:5;1:5].', 1), [12, 12])
%!assert (trapz ([1:5;1:5], 2), [12; 12])
%!assert (trapz (repmat (reshape (1:5,1,1,5),2,2), 3), [12 12; 12 12])
%!assert (trapz ([0:0.5:2;1:5].', [1:5;1:5].', 1), [6, 12])
%!assert (trapz ([0:0.5:2;1:5], [1:5;1:5], 2), [6; 12])
%!assert (trapz (repmat (reshape ([0:0.5:2],1,1,5),2,2), ...
%!               repmat (reshape (1:5,1,1,5),2,2), 3), [6 6; 6 6])
%!assert (trapz (0:0.5:2, [(1:5)', (1:5)']), [6, 6])
%!assert (trapz (0:0.5:2, [(1:5); (1:5)], 2), [6; 6])
%!assert (trapz (0:0.5:2, repmat (reshape (1:5,1,1,5),2,2),3), [6 6; 6 6])
%!assert <*54277> (trapz (ones (1,3), 1), zeros (1,3))
%!assert <*54277> (trapz (ones (3,1), 2), zeros (3,1))

## Test input validation
%!error <Invalid call> trapz ()
%!error <DIM must be an integer> trapz (1, 2, [1 2])
%!error <DIM must be an integer> trapz (1, 2, 1.5)
%!error <DIM must be .* a valid dimension> trapz (1, 2, 0)
%!error <length of X and length of Y.*must match> trapz ([1 2], [1 2 3])
%!error <X and Y must have same shape> trapz (ones (2,3), ones (2,4))
