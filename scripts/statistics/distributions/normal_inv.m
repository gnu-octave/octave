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
## @deftypefn {Function File} {} normal_inv (@var{x}, @var{m}, @var{v})
## For each element of @var{x}, compute the quantile (the inverse of the
## CDF) at @var{x} of the normal distribution with mean @var{m} and
## variance @var{v}.
##
## Default values are @var{m} = 0, @var{v} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Quantile function of the normal distribution

function inv = normal_inv (x, m, v)

  if (! ((nargin == 1) || (nargin == 3)))
    usage ("normal_inv (x, m, v)");
  endif

  if (nargin == 1)
    m = 0;
    v = 1;
  endif

  [retval, x, m, v] = common_size (x, m, v);
  if (retval > 0)
    error ("normal_inv: x, m and v must be of common size or scalars");
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  m = reshape (m, 1, s);
  v = reshape (v, 1, s);
  inv = zeros (1, s);

  k = find (isinf (m) | isnan (m) | !(v >= 0) | !(v < Inf));
  if (any (k))
    inv(k) = NaN * ones (1, length (k));
  endif

  k = find (!isinf (m) & !isnan (m) & (v > 0) & (v < Inf));
  if (any (k))
    inv(k) = m(k) + sqrt (v(k)) .* stdnormal_inv (x(k));
  endif

  inv = reshape (inv, r, c);

endfunction
