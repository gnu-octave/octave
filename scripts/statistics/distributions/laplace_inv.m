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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} laplace_inv (@var{x})
## For each element of @var{x}, compute the quantile (the inverse of the
## CDF) at @var{x} of the Laplace distribution.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the Laplace distribution

function inv = laplace_inv (x)

  if (nargin != 1)
    usage ("laplace_inv (x)");
  endif

  inv = (-Inf) * ones (size (x));

  k = find (isnan (x) | (x < 0) | (x > 1));
  if (any (k))
    inv(k) = NaN;
  endif

  k = find (x == 1);
  if (any (k))
    inv(k) = Inf;
  endif

  k = find ((x > 0) & (x < 1));
  if (any (k))
    inv(k) = ((x(k) < 1/2) .* log (2 * x(k))
	      - (x(k) > 1/2) .* log (2 * (1 - x(k))));
  endif

endfunction
