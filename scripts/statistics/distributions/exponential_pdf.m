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
## @deftypefn {Function File} {} exponential_pdf (@var{x}, @var{lambda})
## For each element of @var{x}, compute the probability density function
## (PDF) of the exponential distribution with parameter @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: PDF of the exponential distribution

function pdf = exponential_pdf (x, l)

  if (nargin != 2)
    usage ("exponential_pdf (x, lambda)");
  endif

  [retval, x, l] = common_size (x, l);
  if (retval > 0)
    error ("exponential_pdf: x and lambda must be of common size or scalar");
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  l = reshape (l, 1, s);
  pdf = zeros (1, s);

  k = find (!(l > 0) | isnan (x));
  if (any (k))
    pdf(k) = NaN * ones (1, length (k));
  endif

  k = find ((x > 0) & (x < Inf) & (l > 0));
  if (any (k))
    pdf(k) = l(k) .* exp (- l(k) .* x(k));
  endif

  pdf = reshape (pdf, r, c);

endfunction
