## Copyright (C) 1995, 1996, 1997  Friedrich Leisch
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

## -*- texinfo -*-
## @deftypefn {Function File} {} spectral_xdf (@var{x}, @var{win}, @var{b})
## Return the spectral density estimator given a data vector @var{x},
## window name @var{win}, and bandwidth, @var{b}.
##
## The window name, e.g., @code{"triangle"} or @code{"rectangle"} is
## used to search for a function called @code{@var{win}_sw}.
##
## If @var{win} is omitted, the triangle window is used.  If @var{b} is
## omitted, @code{1 / sqrt (length (@var{x}))} is used.
## @end deftypefn

## Author:  FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description:  Spectral density estimation

function retval = spectral_xdf (X, win, b)

  xr = length (X);

  if (columns (X) > 1)
    X = X';
  endif

  if (nargin < 3)
    b = 1 / ceil (sqrt (xr));
  endif

  if (nargin == 1)
    w = triangle_sw (xr, b);
  else
    win = [win, "_sw"];
    w = feval (win, xr, b);
  endif

  X = X - sum (X) / xr;

  retval = (abs (fft (X)) / xr).^2;
  retval = real (ifft (fft(retval) .* fft(w)));

  retval = [(zeros (xr, 1)), retval];
  retval(:, 1) = (0 : xr-1)' / xr;

endfunction








