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
## @deftypefn {Function File} {} stdnormal_pdf (@var{x})
## For each element of @var{x}, compute the probability density function
## (PDF) of the standard normal distribution at @var{x}.
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: PDF of the standard normal distribution

function pdf = stdnormal_pdf (x)

  if (nargin != 1)
    usage ("stdnormal_pdf (x)");
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  pdf = zeros (1, s);

  k = find (isnan (x));
  if (any (k))
    pdf(k) = NaN * ones (1, length (k));
  endif

  k = find (!isinf (x));
  if (any (k))
    pdf (k) = (2 * pi)^(- 1/2) * exp (- x(k) .^ 2 / 2);
  endif

  pdf = reshape (pdf, r, c);

endfunction