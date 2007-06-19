## Copyright (C) 2001,2002  Kai Habel
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{pp} = } pchip (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{yi} = } pchip (@var{x}, @var{y}, @var{xi})
##
## Piecewise Cubic Hermite interpolating polynomial. Called with two
## arguments, the piece-wise polynomial @var{pp} is returned, that may
## later be used with @code{ppval} to evaluate the polynomial at
## specific points.
##
## The variable @var{x} must be a strictly monotonic vector (either
## increasing or decreasing). While @var{y} can be either a vector or
## array. In the case where @var{y} is a vector, it must have a length
## of @var{n}. If @var{y} is an array, then the size of @var{y} must
## have the form
## @iftex
## @tex
## $$[s_1, s_2, \cdots, s_k, n]$$
## @end tex
## @end iftex
## @ifinfo
## @code{[@var{s1}, @var{s2}, @dots{}, @var{sk}, @var{n}]}
## @end ifinfo
## The array is then reshaped internally to a matrix where to leading
## dimension is given by 
## @iftex
## @tex
## $$s_1 s_2 \cdots s_k$$
## @end tex
## @end iftex
## @ifinfo
## @code{@var{s1} * @var{s2} * @dots{} * @var{sk}}
## @end ifinfo
## and each row this matrix is then treated seperately. Note that this
## is exactly the opposite treatment than @code{interp1} and is done
## for compatiability.
##
## Called with a third input argument, @code{pchip} evaluates the 
## piece-wise polynomial at the points @var{xi}. There is an equivalence
## between @code{ppval (pchip (@var{x}, @var{y}), @var{xi})} and
## @code{pchip (@var{x}, @var{y}, @var{xi})}.
##
## @seealso{spline, ppval, mkpp, unmkpp}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>
## Date: 9. mar 2001
##
## S_k = a_k + b_k*x + c_k*x^2 + d_k*x^3; (spline polynom)
##
## 4 conditions:
## S_k(x_k) = y_k;
## S_k(x_k+1) = y_k+1;
## S_k'(x_k) = y_k';
## S_k'(x_k+1) = y_k+1';

function ret = pchip (x, y, xi)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  x = x(:);
  n = length (x);

  ## Check the size and shape of y
  ndy = ndims (y);
  szy = size (y);
  if (ndy == 2 && (szy(1) == 1 || szy(2) == 1))
    if (szy(1) == 1)
      y = y';
    else
      szy = fliplr (szy);
    endif
  else
    y = reshape (y, [prod(szy(1:end-1)), szy(end)])';
  endif

  h = diff (x);
  if (all (h < 0))
    x = flipud (x);
    h = diff (x);
    y = flipud (y);
  elseif (any (h <= 0))
    error("pchip: x must be strictly monotonic")
  endif

  if (rows (y) != n)
    error("pchip: size of x and y must match");
  endif

  [ry, cy] = size (y);
  if (cy > 1)
    h = kron (diff (x), ones (1, cy));
  endif
  
  dy = diff (y) ./ h;

  a = y;
  b = __pchip_deriv__ (x, y);
  c = - (b(2:n, :) + 2 * b(1:n - 1, :)) ./ h + 3 * diff (a) ./ h .^ 2;
  d = (b(1:n - 1, :) + b(2:n, :)) ./ h.^2 - 2 * diff (a) ./ h.^3;

  d = d(1:n - 1, :); c = c(1:n - 1, :);
  b = b(1:n - 1, :); a = a(1:n - 1, :);
  coeffs = [d(:), c(:), b(:), a(:)];
  pp = mkpp (x, coeffs, szy(1:end-1));

  if (nargin == 2)
    ret = pp;
  else
    ret = ppval (pp, xi);
  endif

endfunction

%!demo
%! x = 0:8; 
%! y = [1, 1, 1, 1, 0.5, 0, 0, 0, 0];
%! xi = 0:0.01:8; 
%! yspline = spline(x,y,xi);
%! ypchip = pchip(x,y,xi);
%! title("pchip and spline fit to discontinuous function");
%! plot(xi,yspline,xi,ypchip,"-",x,y,"+");
%! legend ("spline","pchip","data");
%! %-------------------------------------------------------------------
%! % confirm that pchip agreed better to discontinuous data than spline

%!shared x,y
%! x = 0:8; 
%! y = [1, 1, 1, 1, 0.5, 0, 0, 0, 0];
%!assert (pchip(x,y,x), y);
%!assert (pchip(x,y,x'), y');
%!assert (pchip(x',y',x'), y');
%!assert (pchip(x',y',x), y);
%!assert (isempty(pchip(x',y',[])));
%!assert (isempty(pchip(x,y,[])));
%!assert (pchip(x,[y;y],x), [pchip(x,y,x);pchip(x,y,x)])
