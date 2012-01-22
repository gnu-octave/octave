## Copyright (C) 1996-2012 John W. Eaton
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
## @deftypefn  {Function File} {@var{p} =} polyfit (@var{x}, @var{y}, @var{n})
## @deftypefnx {Function File} {[@var{p}, @var{s}] =} polyfit (@var{x}, @var{y}, @var{n})
## @deftypefnx {Function File} {[@var{p}, @var{s}, @var{mu}] =} polyfit (@var{x}, @var{y}, @var{n})
## Return the coefficients of a polynomial @var{p}(@var{x}) of degree
## @var{n} that minimizes the least-squares-error of the fit to the points
## @code{[@var{x}, @var{y}]}.
##
## The polynomial coefficients are returned in a row vector.
##
## The optional output @var{s} is a structure containing the following fields:
##
## @table @samp
## @item R
## Triangular factor R from the QR@tie{}decomposition.
##
## @item X
## The Vandermonde matrix used to compute the polynomial coefficients.
##
## @item df
## The degrees of freedom.
##
## @item normr
## The norm of the residuals.
##
## @item yf
## The values of the polynomial for each value of @var{x}.
## @end table
##
## The second output may be used by @code{polyval} to calculate the
## statistical error limits of the predicted values.
##
## When the third output, @var{mu}, is present the
## coefficients, @var{p}, are associated with a polynomial in
## @var{xhat} = (@var{x}-@var{mu}(1))/@var{mu}(2).
## Where @var{mu}(1) = mean (@var{x}), and @var{mu}(2) = std (@var{x}).
## This linear transformation of @var{x} improves the numerical
## stability of the fit.
## @seealso{polyval, polyaffine, roots, vander, zscore}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 13 December 1994
## Adapted-By: jwe

function [p, s, mu] = polyfit (x, y, n)

  if (nargin < 3 || nargin > 4)
    print_usage ();
  endif

  if (nargout > 2)
    ## Normalized the x values.
    mu = [mean(x), std(x)];
    x = (x - mu(1)) / mu(2);
  endif

  if (! size_equal (x, y))
    error ("polyfit: X and Y must be vectors of the same size");
  endif

  if (! (isscalar (n) && n >= 0 && ! isinf (n) && n == fix (n)))
    error ("polyfit: N must be a non-negative integer");
  endif

  y_is_row_vector = (rows (y) == 1);

  ## Reshape x & y into column vectors.
  l = numel (x);
  x = x(:);
  y = y(:);

  ## Construct the Vandermonde matrix.
  v = vander (x, n+1);

  ## Solve by QR decomposition.
  [q, r, k] = qr (v, 0);
  p = r \ (q' * y);
  p(k) = p;

  if (nargout > 1)
    yf = v*p;

    if (y_is_row_vector)
      s.yf = yf.';
    else
      s.yf = yf;
    endif

    s.R = r;
    s.X = v;
    s.df = l - n - 1;
    s.normr = norm (yf - y);
  endif

  ## Return a row vector.
  p = p.';

endfunction

%!test
%! x = [-2, -1, 0, 1, 2];
%! assert(all (all (abs (polyfit (x, x.^2+x+1, 2) - [1, 1, 1]) < sqrt (eps))));

%!error(polyfit ([1, 2; 3, 4], [1, 2, 3, 4], 2))

%!test
%! x = [-2, -1, 0, 1, 2];
%! assert(all (all (abs (polyfit (x, x.^2+x+1, 3) - [0, 1, 1, 1]) < sqrt (eps))));

%!test
%! x = [-2, -1, 0, 1, 2];
%! fail("polyfit (x, x.^2+x+1)");

%!test
%! x = [-2, -1, 0, 1, 2];
%! fail("polyfit (x, x.^2+x+1, [])");

## Test difficult case where scaling is really needed. This example
## demonstrates the rather poor result which occurs when the dependent
## variable is not normalized properly.
## Also check the usage of 2nd & 3rd output arguments.
%!test
%! x = [ -1196.4, -1195.2, -1194, -1192.8, -1191.6, -1190.4, -1189.2, -1188, \
%!       -1186.8, -1185.6, -1184.4, -1183.2, -1182];
%! y = [ 315571.7086, 315575.9618, 315579.4195, 315582.6206, 315585.4966,    \
%!       315588.3172, 315590.9326, 315593.5934, 315596.0455, 315598.4201,    \
%!       315600.7143, 315602.9508, 315605.1765 ];
%! [p1, s1] = polyfit (x, y, 10);
%! [p2, s2, mu] = polyfit (x, y, 10);
%! assert (s2.normr < s1.normr)

%!test
%! x = 1:4;
%! p0 = [1i, 0, 2i, 4];
%! y0 = polyval (p0, x);
%! p = polyfit (x, y0, numel(p0)-1);
%! assert (p, p0, 1000*eps)

%!test
%! x = 1000 + (-5:5);
%! xn = (x - mean (x)) / std (x);
%! pn = ones (1,5);
%! y = polyval (pn, xn);
%! [p, s, mu] = polyfit (x, y, numel(pn)-1);
%! [p2, s2] = polyfit (x, y, numel(pn)-1);
%! assert (p, pn, s.normr)
%! assert (s.yf, y, s.normr)
%! assert (mu, [mean(x), std(x)])
%! assert (s.normr/s2.normr < sqrt(eps))

%!test
%! x = [1, 2, 3; 4, 5, 6];
%! y = [0, 0, 1; 1, 0, 0];
%! p = polyfit (x, y, 5);
%! expected = [0, 1, -14, 65, -112, 60]/12;
%! assert (p, expected, sqrt(eps))


