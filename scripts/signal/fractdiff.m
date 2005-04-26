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
## @deftypefn {Function File} {} fractdiff (@var{x}, @var{d})
## Compute the fractional differences @math{(1-L)^d x} where @math{L}
## denotes the lag-operator and @math{d} is greater than -1.
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Compute fractional differences

function retval = fractdiff (x, d)

  N = 100;

  if (! isvector (x))
    error ("fractdiff: x must be a vector")
  endif

  if (! isscalar (d))
    error ("fractdiff: d must be a scalar")
  endif


  if (d >= 1)
    for k = 1 : d
      x = x(2 : length (x)) - x(1 : length (x) - 1);
    endfor
  endif

  if (d > -1)

    d = rem (d, 1);

    if (d != 0)
      n = (0 : N)';
      w = real (gamma (-d+n) ./ gamma (-d) ./ gamma (n+1));
      retval = fftfilt (w, x);
      retval = retval(1 : length (x));
    else
      retval = x;
    endif

  else
    error ("fractdiff: d must be > -1");

  endif

endfunction
