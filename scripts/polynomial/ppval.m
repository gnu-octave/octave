## Copyright (C) 2000-2011 Paul Kienzle
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{yi} =} ppval (@var{pp}, @var{xi})
## Evaluate piecewise polynomial @var{pp} at the points @var{xi}.
## If @var{pp} is scalar-valued, the result is an array of the same shape as
## @var{xi}.
## Otherwise, the size of the result is @code{[pp.d, length(@var{xi})]} if
## @var{xi} is a vector, or @code{[pp.d, size(@var{xi})]} if it is a
## multi-dimensional array.  If pp.orient is 1, the dimensions are permuted as
## in interp1, to
## @code{[pp.d, length(@var{xi})]} and @code{[pp.d, size(@var{xi})]}
## respectively.
## @seealso{mkpp, unmkpp, spline}
## @end deftypefn

function yi = ppval (pp, xi)

  if (nargin != 2)
    print_usage ();
  endif
  if (! isstruct (pp))
    error ("ppval: PP must be a structure");
  endif

  ## Extract info.
  x = pp.x;
  P = pp.P;
  d = pp.d;
  k = size (P, 3);
  nd = size (P, 1);

  ## Determine resulting shape.
  if (d == 1) # scalar case
    yisz = size (xi);
  elseif (isvector (xi)) # this is special
    yisz = [d, length(xi)];
  else # general
    yisz = [d, size(xi)];
  endif

  ## Determine intervals.
  xi = xi(:);
  xn = numel (xi);

  idx = lookup (x, xi, "lr");

  ## Offsets.
  dx = (xi - x(idx)).';
  dx = dx(ones (1, nd), :); # spread (do nothing in 1D)

  ## Use Horner scheme.
  yi = P(:,idx,1);
  for i = 2:k;
    yi .*= dx;
    yi += P(:,idx,i);
  endfor

  ## Adjust shape.
  yi = reshape (yi, yisz);
  if (d != 1 && pp.orient == 1)
    ## Switch dimensions to match interp1 order.
    yi = shiftdim (yi, length (d));
  endif

endfunction
