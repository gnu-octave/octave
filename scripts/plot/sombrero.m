## Copyright (C) 1996 John W. Eaton
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## usage: sombrero (n)
##
## Draw a `sombrero' in three dimensions using n grid lines.  The
## function plotted is
##
##   z = sin (x^2 + y^2) / (x^2 + y^2);

## Author: jwe

function sombrero (n)

  if (nargin != 1)
    usage ("sombrero (n)");
  endif

  x = y = linspace (-8, 8, n)';
  [xx, yy] = meshdom (x, y);
  r = sqrt (xx .^ 2 + yy .^ 2) + eps;
  z = sin (r) ./ r;

  mesh (x, y, z);

endfunction
