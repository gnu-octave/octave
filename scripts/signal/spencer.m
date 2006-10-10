## Copyright (C) 1995, 1996, 1997  Friedrich Leisch
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
## @deftypefn {Function File} {} spencer (@var{x})
## Return Spencer's 15 point moving average of every single column of
## @var{x}.
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Apply Spencer's 15-point MA filter

function retval = spencer (X)

  if (nargin != 1)
    print_usage ();
  endif

  [xr, xc] = size(X);

  n = xr;
  c = xc;

  if (isvector(X))
   n = length(X);
   c = 1;
   X = reshape(X, n, 1);
  endif

  W = [-3, -6, -5, 3, 21, 46, 67, 74, 67, 46, 21, 3, -5, -6, -3] / 320;

  retval = fftfilt (W, X);
  retval = [zeros(7,c); retval(15:n,:); zeros(7,c);];

  retval = reshape(retval, xr, xc);

endfunction











