## Copyright (C) 1995, 1996, 1997  Andreas Weingessel
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## usage:  sinewave (m, n [, d])
##
## Computes an (m x 1)-vector X with i-th element X(i) given by sin (2 *
## pi * (i+d-1) / n).
##
## The default value for d is 0.

## Author:  AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Description:  Compute a sine wave

function x = sinewave (m, n, d)

  if (nargin == 2)
    d = 0;
  elseif (nargin != 3)
    usage ("sinewave (m, n [, d])");
  endif

  x = sin ( ((1 : m) + d - 1) * 2 * pi / n);

endfunction
