## Copyright (C) 1995, 1996, 1997  Kurt Hornik
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
## @deftypefn {Function File} {} chisquare_inv (@var{x}, @var{n})
## For each element of @var{x}, compute the quantile (the inverse of the
## CDF) at @var{x} of the chisquare distribution with @var{n} degrees of
## freedom.
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: Quantile function of the chi-square distribution

function inv = chisquare_inv (x, n)

  if (nargin != 2)
    usage ("chisquare_inv (x, n)");
  endif

  [retval, x, n] = common_size (x, n);
  if (retval > 0)
    error ("chisquare_inv: x and n must be of common size or scalar");
  endif

  inv = gamma_inv (x, n / 2, 1 / 2);

  ## Allow only for (positive) integer n.
  k = find (n != round (n));
  if (any (k))
    warning ("chisquare_inv: n should be positive integer");
    [r, c] = size (x);
    inv = reshape (inv, 1, r * c);
    inv(k) = NaN * ones (1, length (k));
    inv = reshape (inv, r, c);
  endif

endfunction
