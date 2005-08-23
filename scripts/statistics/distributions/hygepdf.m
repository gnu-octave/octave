## Copyright (C) 1996, 1997  Kurt Hornik
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
## @deftypefn {Function File} {} hygepdf (@var{x}, @var{m}, @var{t}, @var{n})
## Compute the probability density function (PDF) at @var{x} of the
## hypergeometric distribution with parameters @var{m}, @var{t}, and
## @var{n}. This is the probability of obtaining @var{x} marked items
## when randomly drawing a sample of size @var{n} without replacement
## from a population of total size @var{t} containing @var{m} marked items.
##
## The arguments must be of common size or scalar.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the hypergeometric distribution

function pdf = hygepdf (x, m, t, n)

  if (nargin != 4)
    usage ("hygepdf (x, m, t, n)");
  endif

  if (!isscalar (m) || !isscalar (t) || !isscalar (n))
    [retval, x, m, t, n] = common_size (x, m, t, n);
    if (retval > 0)
      error ("hygepdf: x, m, t, and n must be of common size or scalar");
    endif
  endif

  pdf = zeros (size (x));

  ## everything in i1 gives NaN
  i1 = ((m < 0) | (t < 0) | (n <= 0) | (m != round (m)) |
        (t != round (t)) | (n != round (n)) | (m > t) | (n > t));
  ## everything in i2 gives 0 unless in i1
  i2 = ((x != round (x)) | (x < 0) | (x > m) | (n < x) | (n-x > t-m));
  k = find (i1);
  if (any (k))
    if (isscalar (m) && isscalar (t) && isscalar (n))
      pdf = NaN * ones ( size (x));
    else
      pdf (k) = NaN;
    endif
  endif
  k = find (!i1 & !i2);
  if (any (k))
    if (isscalar (m) && isscalar (t) && isscalar (n))
      pdf (k) = (bincoeff (m, x(k)) .* bincoeff (t-m, n-x(k))
		 / bincoeff (t, n));
    else
      pdf (k) = (bincoeff (m(k), x(k)) .* bincoeff (t(k)-m(k), n(k)-x(k))
		 ./ bincoeff (t(k), n(k)));
    endif
  endif

endfunction
