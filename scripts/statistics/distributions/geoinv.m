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
## @deftypefn {Function File} {} geoinv (@var{x}, @var{p})
## For each element of @var{x}, compute the quantile at @var{x} of the
## geometric distribution with parameter @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Quantile function of the geometric distribution

function inv = geoinv (x, p)

  if (nargin != 2)
    usage ("geoinv (x, p)");
  endif

  if (!isscalar (x) && !isscalar (p))
    [retval, x, p] = common_size (x, p);
    if (retval > 0)
      error ("geoinv: x and p must be of common size or scalar");
    endif
  endif

  inv = zeros (size (x));

  k = find (!(x >= 0) | !(x <= 1) | !(p >= 0) | !(p <= 1));
  if (any (k))
    inv(k) = NaN;
  endif

  k = find ((x == 1) & (p >= 0) & (p <= 1));
  if (any (k))
    inv(k) = Inf;
  endif

  k = find ((x > 0) & (x < 1) & (p > 0) & (p <= 1));
  if (any (k))
    if (isscalar (x))
      inv(k) = max (ceil (log (1 - x) ./ log (1 - p(k))) - 1, 0);
    elseif (isscalar (p))
      inv(k) = max (ceil (log (1 - x(k)) / log (1 - p)) - 1, 0);
    else
      inv(k) = max (ceil (log (1 - x(k)) ./ log (1 - p(k))) - 1, 0);
    endif
  endif

endfunction
