## Copyright (C) 1993-2012 John W. Eaton
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
## @deftypefn  {Function File} {} sombrero ()
## @deftypefnx {Function File} {} sombrero (@var{n})
## Produce the familiar three-dimensional sombrero plot using @var{n}
## grid lines.  If @var{n} is omitted, a value of 41 is assumed.
##
## The function plotted is
##
## @example
## z = sin (sqrt (x^2 + y^2)) / (sqrt (x^2 + y^2))
## @end example
## @seealso{surf, meshgrid, mesh}
## @end deftypefn

## Author: jwe

function [x, y, z] = sombrero (n = 41)

  if (nargin > 2)
    print_usage ();
  elseif (n <= 1)
    error ("sombrero: number of grid lines N must be greater than 1");
  endif

  tx = linspace (-8, 8, n)';
  ty = tx;
  [xx, yy] = meshgrid (tx, ty);
  r = sqrt (xx .^ 2 + yy .^ 2) + eps;
  tz = sin (r) ./ r;
  if (nargout == 0)
    surf (tx, ty, tz);
    box ("off");
  else
    x = tx;
    y = ty;
    z = tz;
  endif

endfunction


%!demo
%! clf;
%! colormap ('default');
%! sombrero ();

## Test input validation
%!error sombrero (1,2,3)
%!error <N must be greater than 1> sombrero (1)

