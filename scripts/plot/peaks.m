## Copyright (C) 2007-2012 Paul Kienzle
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
## @deftypefn  {Function File} {} peaks ()
## @deftypefnx {Function File} {} peaks (@var{n})
## @deftypefnx {Function File} {} peaks (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{z} =} peaks (@dots{})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{z}] =} peaks (@dots{})
## Generate a function with lots of local maxima and minima.  The function
## has the form
##
## @tex
## $f(x,y) = 3 (1 - x) ^ 2 e ^ {\left(-x^2 - (y+1)^2\right)} - 10 \left({x \over 5} - x^3 - y^5)\right) - {1 \over 3} e^{\left(-(x+1)^2 - y^2\right)}$
## @end tex
## @ifnottex
## @verbatim
## f(x,y) = 3*(1-x)^2*exp(-x^2 - (y+1)^2) ...
##          - 10*(x/5 - x^3 - y^5)*exp(-x^2-y^2) ...
##          - 1/3*exp(-(x+1)^2 - y^2)
## @end verbatim
## @end ifnottex
##
## Called without a return argument, @code{peaks} plots the surface of the
## above function using @code{mesh}.  If @var{n} is a scalar, the @code{peaks}
## returns the values of the above function on a @var{n}-by-@var{n} mesh over
## the range @code{[-3,3]}.  The default value for @var{n} is 49.
##
## If @var{n} is a vector, then it represents the @var{x} and @var{y} values
## of the grid on which to calculate the above function.  The @var{x} and
## @var{y} values can be specified separately.
## @seealso{surf, mesh, meshgrid}
## @end deftypefn

## Expression for the peaks function was taken from the following paper:
## http://www.control.hut.fi/Kurssit/AS-74.115/Material/GENALGgoga.pdf

function [X_out, Y_out, Z_out] = peaks (x, y)

  if (nargin == 0)
    x = y = linspace (-3, 3, 49);
  elseif (nargin == 1)
    if length(x) > 1
      y = x;
    else
      x = y = linspace (-3, 3, x);
    endif
  endif

  if (isvector (x) && isvector (y))
    [X, Y] = meshgrid (x, y);
  else
    X = x;
    Y = y;
  endif

  Z = 3 * (1 - X) .^ 2 .* exp(- X .^ 2 - (Y + 1) .^ 2) \
      - 10 * (X / 5 - X .^ 3 - Y .^ 5) .* exp(- X .^ 2 - Y .^ 2) \
      - 1 / 3 * exp(- (X + 1) .^ 2 - Y .^ 2);

  if (nargout == 0)
    surf (x, y, Z);
  elseif (nargout == 1)
    X_out = Z;
  else
    X_out = X;
    Y_out = Y;
    Z_out = Z;
  endif

endfunction
