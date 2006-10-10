## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} sombrero (@var{n})
##
## Draw a `sombrero' in three dimensions using @var{n} grid lines.  The
## function plotted is
##
## @example
##   z = sin (sqrt (x^2 + y^2)) / (sqrt (x^2 + y^2))
## @end example
## @end deftypefn

## Author: jwe

function sombrero (n)

  if (nargin == 0)
    n = 41;
  endif

  if (nargin < 2)
    if (n > 1)
      x = y = linspace (-8, 8, n)';
      [xx, yy] = meshgrid (x, y);
      r = sqrt (xx .^ 2 + yy .^ 2) + eps;
      z = sin (r) ./ r;
      mesh (x, y, z);
    else
      error ("sombrero: number of grid lines must be greater than 1");
    endif
  else
    print_usage ();
  endif

endfunction
