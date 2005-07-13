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
## @deftypefn {Function File} {} norminv (@var{x}, @var{m}, @var{v})
## For each element of @var{x}, compute the quantile (the inverse of the
## CDF) at @var{x} of the normal distribution with mean @var{m} and
## variance @var{v}.
##
## Default values are @var{m} = 0, @var{v} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Quantile function of the normal distribution

function inv = norminv (x, m, v)

  if (nargin != 1 && nargin != 3)
    usage ("norminv (x, m, v)");
  endif

  if (nargin == 1)
    m = 0;
    v = 1;
  endif

  if (!isscalar (m) || !isscalar(v))
    [retval, x, m, v] = common_size (x, m, v);
    if (retval > 0)
      error ("norminv: x, m and v must be of common size or scalars");
    endif
  endif

  sz = size (x);
  inv = zeros (sz);

  if (isscalar (m) && isscalar(v))
    if (find (isinf (m) | isnan (m) | !(v > 0) | !(v < Inf)))
      inv = NaN * ones (sz);
    else
      inv =  m + sqrt (v) .* stdnormal_inv (x);
    endif
  else
    k = find (isinf (m) | isnan (m) | !(v > 0) | !(v < Inf));
    if (any (k))
      inv(k) = NaN;
    endif

    k = find (!isinf (m) & !isnan (m) & (v > 0) & (v < Inf));
    if (any (k))
      inv(k) = m(k) + sqrt (v(k)) .* stdnormal_inv (x(k));
    endif
  endif

  k = find ((v == 0) & (x > 0) & (x < 1));
  if (any (k))
    inv(k) = m(k);
  endif

  inv((v == 0) & (x == 0)) = -Inf;
  inv((v == 0) & (x == 1)) = Inf;

endfunction
