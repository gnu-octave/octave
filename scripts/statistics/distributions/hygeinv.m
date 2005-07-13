## Copyright (C) 1997  Kurt Hornik
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
## @deftypefn {Function File} {} hygeinv (@var{x}, @var{m}, @var{t}, @var{n})
## For each element of @var{x}, compute the quantile at @var{x} of the
## hypergeometric distribution with parameters @var{m}, @var{t}, and
## @var{n}.
##
## The parameters @var{m}, @var{t}, and @var{n} must positive integers
## with @var{m} and @var{n} not greater than @var{t}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Random deviates from the hypergeometric distribution

function inv = hygeinv (x, m, t, n)

  if (nargin != 4)
    usage ("hygeinv (x, m, t, n)");
  endif

  if (!isscalar (m) || !isscalar (t) || !isscalar (n))
    error ("hygeinv: m, t and n must all be positive integers");
  endif

  if ((m < 0) | (t < 0) | (n <= 0) | (m != round (m)) |
      (t != round (t)) | (n != round (n)) | (m > t) | (n > t))
    inv = NaN * ones (size (x))
  else
    inv = discrete_inv (x, 0 : n, hygepdf (0 : n, m, t, n));
  endif

endfunction
