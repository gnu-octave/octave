## Copyright (C) 2001-2011 Kai Habel
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
## @deftypefn  {Function File} {@var{pp} =} pchip (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{yi} =} pchip (@var{x}, @var{y}, @var{xi})
##
## Piecewise Cubic Hermite interpolating polynomial.  Called with two
## arguments, the piecewise polynomial @var{pp} is returned, that may
## later be used with @code{ppval} to evaluate the polynomial at
## specific points.
##
## The variable @var{x} must be a strictly monotonic vector (either
## increasing or decreasing).  While @var{y} can be either a vector or
## array.  In the case where @var{y} is a vector, it must have a length
## of @var{n}.  If @var{y} is an array, then the size of @var{y} must
## have the form
## @tex
## $$[s_1, s_2, \cdots, s_k, n]$$
## @end tex
## @ifnottex
## @code{[@var{s1}, @var{s2}, @dots{}, @var{sk}, @var{n}]}
## @end ifnottex
## The array is then reshaped internally to a matrix where the leading
## dimension is given by 
## @tex
## $$s_1 s_2 \cdots s_k$$
## @end tex
## @ifnottex
## @code{@var{s1} * @var{s2} * @dots{} * @var{sk}}
## @end ifnottex
## and each row in this matrix is then treated separately.  Note that this
## is exactly the opposite treatment than @code{interp1} and is done
## for compatibility.
##
## Called with a third input argument, @code{pchip} evaluates the 
## piecewise polynomial at the points @var{xi}.  There is an equivalence
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

  x = x(:).';
  n = length (x);

  ## Check the size and shape of y
  if (isvector (y))
    y = y(:).';
    szy = size (y);
  else
    szy = size (y);
    y = reshape (y, [prod(szy(1:end-1)), szy(end)]);
  endif

  h = diff (x);
  if (all (h < 0))
    x = fliplr (x);
    h = diff (x);
    y = fliplr (y);
  elseif (any (h <= 0))
    error("pchip: X must be strictly monotonic");
  endif

  if (columns (y) != n)
    error("pchip: size of X and Y must match");
  endif

  f1 = y(:,1:n-1);

  ## Compute derivatives.
  d = __pchip_deriv__ (x, y, 2);
  d1 = d(:,1:n-1);
  d2 = d(:,2:n);

  ## This is taken from SLATEC. 
  h = diag (h);

  delta = diff (y, 1, 2) / h;
  del1 = (d1 - delta) / h;
  del2 = (d2 - delta) / h;
  c3 = del1 + del2;
  c2 = -c3 - del1;
  c3 = c3 / h;

  coeffs = cat (3, c3, c2, d1, f1);
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
