## Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2004,
##               2005, 2006, 2007, 2008, 2009 John W. Eaton
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
## @deftypefn  {Function File} {@var{y} =} polyval (@var{p}, @var{x})
## @deftypefnx {Function File} {@var{y} =} polyval (@var{p}, @var{x}, [], @var{mu})
## Evaluate the polynomial @var{p} at the specified values of @var{x}.  When 
## @var{mu} is present evaluate the polynomial for 
## (@var{x}-@var{mu}(1))/@var{mu}(2).
## If @var{x} is a vector or matrix, the polynomial is evaluated for each of
## the elements of @var{x}.
## @deftypefnx {Function File} {[@var{y}, @var{dy}] =} polyval (@var{p}, @var{x}, @var{s})
## @deftypefnx {Function File} {[@var{y}, @var{dy}] =} polyval (@var{p}, @var{x}, @var{s}, @var{mu})
## In addition to evaluating the polynomial, the second output 
## represents the prediction interval, @var{y} +/- @var{dy}, which
## contains at least 50% of the future predictions.  To calculate the
## prediction interval, the structured variable @var{s}, originating
## form `polyfit', must be present.
## @seealso{polyfit, polyvalm, poly, roots, conv, deconv, residue, filter,
## polyderiv, polyint}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function [y, dy] = polyval (p, x, s, mu)

  if (nargin < 2 || nargin > 4 || (nargout == 2 && nargin < 3))
    print_usage ();
  endif

  if (nargin < 3)
    s = [];
  endif

  if (! (isvector (p) || isempty (p)))
    error ("polyval: first argument must be a vector");
  endif

  if (nargin < 4)
    mu = [0, 1];
  endif

  if (isempty (x))
    y = [];
    return;
  endif

  if (length (p) == 0)
    y = p;
    return;
  endif

  n = length (p) - 1;
  k = numel (x);
  x = (x - mu(1)) / mu(2);
  A = (x(:) * ones (1, n+1)) .^ (ones (k, 1) * (n:-1:0));
  y = A * p(:);
  y = reshape (y, size (x));

  if (nargout == 2)
    ## Note: the F-Distribution is generally considered to be single-sided.
    ## http://www.itl.nist.gov/div898/handbook/eda/section3/eda3673.htm
    ##   t = finv (1-alpha, s.df, s.df);
    ##   dy = t * sqrt (1 + sumsq (A/s.R, 2)) * s.normr / sqrt (s.df)
    ## If my inference is correct, then t must equal 1 for polyval.
    ## This is because finv (0.5, n, n) = 1.0 for any n.
    dy = sqrt (1 + sumsq (A/s.R, 2)) * s.normr / sqrt (s.df);
    dy = reshape (dy, size (x));
  endif

endfunction

%!test
%! fail("polyval([1,0;0,1],0:10)");

%!test
%! r = 0:10:50;
%! p = poly (r);
%! p = p / max(abs(p));
%! x = linspace(0,50,11);
%! y = polyval(p,x) + 0.25*sin(100*x);
%! [pf, s] = polyfit (x, y, numel(r));
%! [y1, delta] = polyval (pf, x, s);
%! expected = [0.37235, 0.35854, 0.32231, 0.32448, 0.31328, ...
%!    0.32036, 0.31328, 0.32448, 0.32231, 0.35854, 0.37235];
%! assert (delta, expected, 0.00001)

%!test
%! x = 10 + (-2:2);
%! y = [0, 0, 1, 0, 2];
%! p = polyfit (x, y, numel (x) - 1);
%! [pn, s, mu] = polyfit (x, y, numel (x) - 1);
%! y1 = polyval (p, x);
%! yn = polyval (pn, x, [], mu);
%! assert (y1, y, sqrt(eps))
%! assert (yn, y, sqrt(eps))

%!test
%! p = [0, 1, 0];
%! x = 1:10;
%! assert (x, polyval(p,x), eps)
%! x = x(:);
%! assert (x, polyval(p,x), eps)
%! x = reshape(x, [2, 5]);
%! assert (x, polyval(p,x), eps)
%! x = reshape(x, [5, 2]);
%! assert (x, polyval(p,x), eps)
%! x = reshape(x, [1, 1, 5, 2]);
%! assert (x, polyval(p,x), eps)

