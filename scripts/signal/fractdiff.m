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

## usage:  fractdiff(x, d)
##
## Computes the fractional differences (1-L)^d x where L denotes the
## lag-operator and d > -1.

## Author:  FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description:  Compute fractional differences

function retval = fractdiff (x, d)

  N = 100;

  if !is_vector (x)
    error ("fractdiff:  x must be a vector")
  endif

  if !is_scalar (d)
    error ("fractdiff:  d must be a scalar")
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
    error ("fractdiff:  d must be > -1");

  endif

endfunction
