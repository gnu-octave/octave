## Copyright (C) 1995, 1996, 1997  Kurt Hornik
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

## -*- texinfo -*-
## @deftypefn {Function File} {} uniform_inv (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, compute the quantile (the inverse of the
## CDF) at @var{x} of the uniform distribution on [@var{a}, @var{b}].
##
## Default values are @var{a} = 0, @var{b} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Quantile function of the uniform distribution

function inv = uniform_inv (x, a, b)

  if (! (nargin == 1 || nargin == 3))
    usage ("uniform_inv (x, a, b)");
  endif

  if (nargin == 1)
    a = 0;
    b = 1;
  endif

  [retval, x, a, b] = common_size (x, a, b);
  if (retval > 0)
    error ("uniform_inv: x, a and b must be of common size or scalars");
  endif

  [r, c] = size (x);
  s   = r * c;
  x   = reshape (x, 1, s);
  a   = reshape (a, 1, s);
  b   = reshape (b, 1, s);
  inv = zeros (1, s);

  k = find ((x < 0) | (x > 1) | isnan (x) | !(a < b));
  if (any (k))
    inv(k) = NaN * ones (1, length (k));
  endif

  k = find ((x >= 0) & (x <= 1) & (a < b));
  if (any (k))
    inv(k) = a(k) + x(k) .* (b(k) - a(k));
  endif

  inv = reshape (inv, r, c);

endfunction
