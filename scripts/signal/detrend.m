## Copyright (C) 1995, 1996  Kurt Hornik
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
## @deftypefn {Function File} {} detrend (@var{x}, @var{p})
## If @var{x} is a vector, @code{detrend (@var{x}, @var{p})} removes the
## best fit of a polynomial of order @var{p} from the data @var{x}.
##
## If @var{x} is a matrix, @code{detrend (@var{x}, @var{p})} does the same
## for each column in @var{x}.
##
## The second argument is optional.  If it is not specified, a value of 1
## is assumed.  This corresponds to removing a linear trend.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 11 October 1994
## Adapted-By: jwe

function y = detrend (x, p)

  if (nargin == 1)
    p = 1;
  elseif (nargin == 2)
    if (! (isscalar (p) && p == round (p) && p >= 0))
      error ("detrend: p must be a nonnegative integer");
    endif
  else
    usage ("detrend (x, p)");
  endif

  [m, n] = size (x);
  if (m == 1)
    x = x';
  endif

  r = rows (x);
  b = ((1 : r)' * ones (1, p + 1)) .^ (ones (r, 1) * (0 : p));
  y = x - b * (b \ x);

  if (m == 1)
    y = y';
  endif

endfunction
